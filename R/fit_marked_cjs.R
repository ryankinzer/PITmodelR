#' @title Fit a CJS Model Using `marked`
#'
#' @description
#' Fits a Cormack-Jolly-Seber (CJS) capture-recapture model using the `marked` package
#' and returns tidy results for survival (\code{Phi}) and detection (\code{p}) parameters.
#' Cumulative survival estimates are computed with covariance-aware confidence intervals
#' when possible; otherwise, an independence-based fallback is used.
#'
#' @param ch_data Data frame or tibble with at least the following columns:
#'   \describe{
#'     \item{\code{tag_code}}{Unique identifier for each tagged individual.}
#'     \item{\code{ch}}{Encounter history string (e.g., "11001").}
#'   }
#' @param phi_formula Formula specifying the model for survival (\code{Phi}); default \code{~ time}.
#' @param p_formula Formula specifying the model for detection probability (\code{p}); default \code{~ time}.
#' @param hessian Passed to `marked::crm()`. If `TRUE` (default), compute the Hessian so that uncertainty and variance-covariance matrices can be extracted.
#' @param conf_level Numeric; confidence level for Wald intervals (default 0.95).
#' @param ... Additional arguments passed to marked::crm().
#'
#' @return A list with components:
#' \describe{
#'   \item{\code{model}}{The fitted \code{marked} CJS model object.}
#'   \item{\code{phi}}{Data frame of interval survival estimates with columns \code{estimate}, \code{se}, \code{lcl}, \code{ucl}, and interval index.}
#'   \item{\code{cum_phi}}{Data frame of cumulative survival estimates across intervals with confidence intervals.}
#'   \item{\code{p}}{Data frame of interval detection probabilities with columns \code{estimate}, \code{se}, \code{lcl}, \code{ucl}, and interval index.}
#'   \item{\code{plots}}{List of plots for \code{phi}, \code{p}, and \code{cum_phi}. Uses \code{ggplot2} if available, otherwise base R.}
#'   \item{\code{covariance_mode}}{Character indicating whether cumulative survival used "full" covariance or the "independence_fallback".}
#' }
#'
#' @details
#' This function wraps the `marked::crm` workflow for CJS models, extracts tidy parameter
#' tables, and computes cumulative survival across intervals. If the covariance matrix
#' for \code{Phi} parameters is available, cumulative survival confidence intervals
#' account for correlations; otherwise, a simple product-of-bounds approach is used.
#'
#' @seealso [marked::crm]
#'
#' @note Additional arguments are passed directly to `marked::crm()`. This feature
#'       is experimental and not fully tested; supplying unsupported arguments
#'       may cause errors or unexpected behavior.
#'
#' @author Ryan Kinzer
#'
#' @importFrom marked process.data make.design.data crm
#'
#' @export

fit_marked_cjs <- function(ch_data,
                           phi_formula = ~ time,
                           p_formula   = ~ time,
                           hessian     = TRUE,
                           conf_level  = 0.95,
                           ...) {

  # --- dependencies & validation ---
  if (!requireNamespace("marked", quietly = TRUE)) {
    stop("Package 'marked' must be installed. install.packages('marked')",
         call. = FALSE)
  }

  stopifnot(
    is.data.frame(ch_data),
    all(c("tag_code", "ch") %in% names(ch_data))
  )

  # --- preprocess for marked ---
  proc <- marked::process.data(ch_data, model = "CJS")
  ddl  <- marked::make.design.data(proc)

  Phi.spec <- list(formula = phi_formula)
  p.spec   <- list(formula = p_formula)

  mod <- marked::crm(
    proc,
    ddl,
    model.parameters = list(Phi = Phi.spec, p = p.spec),
    hessian = hessian,
    ...
  )

  z <- stats::qnorm(1 - (1 - conf_level) / 2)

  # --- extractor helper ---
  tidy_reals <- function(mod, param_name) {

    # pull real parameter list
    reals <- mod$results$reals %||% mod$results$real

    # try direct extraction
    tab <- NULL
    if (!is.null(reals) && !is.null(reals[[param_name]])) {
      tab <- as.data.frame(reals[[param_name]], stringsAsFactors = FALSE)

    } else if (!is.null(mod$results$beta)) {
      # fallback prediction
      dd <- if (param_name == "Phi") mod$design.data$Phi
      else                      mod$design.data$p

      pred <- try(
        stats::predict(mod, newdata = dd, type = param_name),
        #marked::predict.crm(mod, newdata = dd, type = param_name),
        silent = TRUE
      )

      if (!inherits(pred, "try-error")) {
        tab <- as.data.frame(pred, stringsAsFactors = FALSE)
      }
    }

    # if still nothing, return empty
    if (is.null(tab)) {
      warning("Could not extract real parameters for '", param_name,
              "'. Returning empty table.", call. = FALSE)

      return(data.frame(
        interval = integer(),
        estimate = numeric(),
        se       = numeric(),
        lcl      = numeric(),
        ucl      = numeric(),
        stringsAsFactors = FALSE
      ))
    }

    # convenience column picker
    pick <- function(options) {
      hit <- intersect(options, names(tab))
      if (length(hit)) tab[[hit[1]]] else rep(NA_real_, nrow(tab))
    }

    est <- as.numeric(pick(c("estimate", "Estimate", "est")))
    se  <- as.numeric(pick(c("se", "SE", "std.error")))
    lcl <- pick(c("lcl", "LCL", "lower", "lower.CL", "lowerCL"))
    ucl <- pick(c("ucl", "UCL", "upper", "upper.CL", "upperCL"))

    # fallback CI if needed
    if (all(is.na(lcl)) || all(is.na(ucl))) {
      lcl <- pmax(0, est - z * se)
      ucl <- pmin(1, est + z * se)
    } else {
      lcl <- as.numeric(lcl)
      ucl <- as.numeric(ucl)
    }

    out <- data.frame(
      interval = seq_len(length(est)),
      estimate = est,
      se       = se,
      lcl      = lcl,
      ucl      = ucl,
      stringsAsFactors = FALSE
    )

    # add time covariate if available
    dd <- if (param_name == "Phi") mod$design.data$Phi
    else                      mod$design.data$p

    if (!is.null(dd) && "time" %in% names(dd)) {
      out$time <- dd$time[seq_len(nrow(out))]
    }

    out
  }

  # extract tables
  phi_df <- tidy_reals(mod, "Phi")
  p_df   <- tidy_reals(mod, "p")

  # --- covariance-aware cumulative survival ---
  cum_phi       <- NULL
  used_full_cov <- FALSE

  try({
    beta_vcv <- mod$results$beta.vcv
    dm_Phi   <- mod$design.matrix$Phi

    if (!is.null(beta_vcv) && !is.null(dm_Phi)) {

      rn <- rownames(beta_vcv)
      phi_idx <- grep("^Phi", rn)

      if (length(phi_idx)) {
        Sigma_beta_phi <- beta_vcv[phi_idx, phi_idx, drop = FALSE]
        X <- as.matrix(dm_Phi)

        if (nrow(X) == nrow(phi_df)) {

          Sigma_eta <- X %*% Sigma_beta_phi %*% t(X)
          Phi_hat   <- pmin(1 - 1e-12, pmax(1e-12, phi_df$estimate))
          v         <- 1 - Phi_hat      # gradient of logPhi wrt eta

          cum_est <- cumprod(Phi_hat)

          k_seq <- seq_len(length(Phi_hat))
          var_logSk <- vapply(
            k_seq,
            function(k) {
              vk  <- v[1:k]
              Sig <- Sigma_eta[1:k, 1:k, drop = FALSE]
              as.numeric(t(vk) %*% Sig %*% vk)
            },
            numeric(1)
          )

          se_logSk <- sqrt(pmax(0, var_logSk))
          lcl <- exp(log(cum_est) - z * se_logSk)
          ucl <- exp(log(cum_est) + z * se_logSk)

          cum_phi <- data.frame(
            interval = phi_df$interval,
            estimate = cum_est,
            lcl      = pmax(0, pmin(1, lcl)),
            ucl      = pmax(0, pmin(1, ucl)),
            stringsAsFactors = FALSE
          )

          used_full_cov <- TRUE
        }
      }
    }
  }, silent = TRUE)

  # --- fallback CI if covariance fails ---
  if (is.null(cum_phi)) {

    if (nrow(phi_df)) {
      cum_phi <- data.frame(
        interval = phi_df$interval,
        estimate = cumprod(pmin(1, pmax(0, phi_df$estimate))),
        lcl      = cumprod(pmin(1, pmax(0, phi_df$lcl))),
        ucl      = cumprod(pmin(1, pmax(0, phi_df$ucl))),
        stringsAsFactors = FALSE
      )

      warning("Used independence fallback for cumulative survival CIs.",
              call. = FALSE)
    } else {
      cum_phi <- data.frame(
        interval = integer(),
        estimate = numeric(),
        lcl = numeric(),
        ucl = numeric()
      )
    }
  }

  # --- plotting helpers ---
  make_ci_plot <- function(df, ylab, title) {
    if (!nrow(df)) return(NULL)

    if (requireNamespace("ggplot2", quietly = TRUE)) {

      ggplot2::ggplot(df, ggplot2::aes(x = interval, y = estimate)) +
        ggplot2::geom_point() +
        ggplot2::geom_errorbar(
          ggplot2::aes(ymin = lcl, ymax = ucl),
          width = 0.2
        ) +
        ggplot2::ylim(0, 1) +
        ggplot2::labs(x = "Interval", y = ylab, title = title) +
        ggplot2::theme_minimal()

    } else {

      function() {
        op <- par(no.readonly = TRUE); on.exit(par(op))

        plot(
          df$interval, df$estimate,
          pch  = 19,
          ylim = c(0, 1),
          xlab = "Interval",
          ylab = ylab,
          main = title
        )

        segments(df$interval, df$lcl, df$interval, df$ucl)
        invisible(NULL)
      }
    }
  }

  plots <- list(
    phi     = make_ci_plot(phi_df, "Survival (Phi)", "Interval Survival (Phi)"),
    p       = make_ci_plot(p_df,   "Detection (p)",  "Interval Detection (p)"),
    cum_phi = make_ci_plot(
      cum_phi,
      "Cumulative Survival",
      paste0(
        "Cumulative Survival to Interval ",
        if (used_full_cov) "(covariance-aware)"
        else               "(independence fallback)"
      )
    )
  )

  # --- return ---
  list(
    model           = mod,
    phi             = phi_df,
    cum_phi         = cum_phi,
    p               = p_df,
    plots           = plots,
    covariance_mode = if (used_full_cov)
      "full"
    else
      "independence_fallback"
  )
}

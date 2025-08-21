#' Fit a CJS model with `marked` and return tidy results + cumulative survival with covariance-aware CIs
#'
#' @param ch_data data frame with columns `tag_code` and `ch`
#' @param phi_formula formula for survival (Phi), default `~ time`
#' @param p_formula   formula for detection (p), default `~ time`
#' @param conf_level  confidence level (default 0.95)
#' @return list(model, phi, cum_phi, p, plots)
#' @export
fit_marked_cjs <- function(ch_data,
                           phi_formula = ~ time,
                           p_formula   = ~ time,
                           conf_level  = 0.95) {
  if (!requireNamespace("marked", quietly = TRUE)) {
    stop("Package 'marked' must be installed. install.packages('marked')", call. = FALSE)
  }
  stopifnot(is.data.frame(ch_data), all(c("tag_code","ch") %in% names(ch_data)))

  proc <- marked::process.data(ch_data, model = "CJS")
  ddl  <- marked::make.design.data(proc)

  Phi.spec <- list(formula = phi_formula)
  p.spec   <- list(formula = p_formula)

  mod <- marked::crm(proc, ddl, model.parameters = list(Phi = Phi.spec, p = p.spec))

  z <- stats::qnorm(1 - (1 - conf_level)/2)

  # ---- Tidy extractors --------------------------------------------------------
  tidy_reals <- function(mod, param_name) {
    reals <- NULL
    if (!is.null(mod$results$reals)) {
      reals <- mod$results$reals
    } else if (!is.null(mod$results$real)) {
      reals <- mod$results$real
    }

    tab <- NULL
    if (!is.null(reals) && !is.null(reals[[param_name]])) {
      tab <- as.data.frame(reals[[param_name]], stringsAsFactors = FALSE)
    } else if (!is.null(mod$results$beta)) {
      dd <- if (param_name == "Phi") mod$design.data$Phi else mod$design.data$p
      pred <- try(marked::predict.crm(mod, newdata = dd, type = param_name), silent = TRUE)
      if (!inherits(pred, "try-error")) tab <- as.data.frame(pred, stringsAsFactors = FALSE)
    }

    if (is.null(tab)) {
      warning("Could not extract real parameters for '", param_name, "'. Returning empty table.", call. = FALSE)
      return(data.frame(interval = integer(), estimate = numeric(), se = numeric(),
                        lcl = numeric(), ucl = numeric(), stringsAsFactors = FALSE))
    }

    pick <- function(options) {
      hit <- intersect(options, names(tab))
      if (length(hit)) tab[[hit[1]]] else rep(NA_real_, nrow(tab))
    }
    est <- as.numeric(pick(c("estimate","Estimate","est")))
    se  <- as.numeric(pick(c("se","SE","std.error")))
    lcl <- pick(c("lcl","LCL","lower","lower.CL","lowerCL"))
    ucl <- pick(c("ucl","UCL","upper","upper.CL","upperCL"))

    if (all(is.na(lcl)) || all(is.na(ucl))) {
      lcl <- pmax(0, est - z * se)
      ucl <- pmin(1, est + z * se)
    } else {
      lcl <- as.numeric(lcl); ucl <- as.numeric(ucl)
    }

    out <- data.frame(
      interval = seq_len(length(est)),
      estimate = est,
      se       = se,
      lcl      = lcl,
      ucl      = ucl,
      stringsAsFactors = FALSE
    )

    dd <- if (param_name == "Phi") mod$design.data$Phi else mod$design.data$p
    if (!is.null(dd) && "time" %in% names(dd)) {
      out$time <- dd$time[seq_len(nrow(out))]
    }

    out
  }

  phi_df <- tidy_reals(mod, "Phi")
  p_df   <- tidy_reals(mod, "p")

  # ---- Covariance-aware cumulative survival on the log scale ------------------
  # Try to compute Var(log S_k) using Σ_eta = X Σ_beta X' for Phi (eta = linear predictor)
  # grad(log Phi) wrt eta is (1 - Phi), so for sum log Phi up to k:
  # Var(log S_k) = v_k' Σ_eta[1:k,1:k] v_k, where v_k = 1 - Phi[1:k]
  cum_phi <- NULL
  used_full_cov <- FALSE
  try({
    beta_vcv <- mod$results$beta.vcv
    dm_Phi   <- mod$design.matrix$Phi
    # pick Phi block in beta.vcv using row/col names (preferred)
    if (!is.null(beta_vcv) && !is.null(dm_Phi)) {
      rn <- rownames(beta_vcv)
      if (!is.null(rn)) {
        phi_idx <- grep("^Phi", rn)
        if (length(phi_idx)) {
          Sigma_beta_phi <- beta_vcv[phi_idx, phi_idx, drop = FALSE]
          X <- as.matrix(dm_Phi)
          # ensure X and phi length align
          if (nrow(X) == nrow(phi_df)) {
            Sigma_eta <- X %*% Sigma_beta_phi %*% t(X)  # cov of eta by interval
            Phi_hat   <- pmax(1e-12, pmin(1 - 1e-12, phi_df$estimate))
            v         <- 1 - Phi_hat                     # gradient for log Phi wrt eta

            cum_est <- cumprod(Phi_hat)

            ck <- seq_len(length(Phi_hat))
            var_logSk <- vapply(ck, function(k) {
              idx <- 1:k
              vk  <- v[idx]
              Sig <- Sigma_eta[idx, idx, drop = FALSE]
              as.numeric(t(vk) %*% Sig %*% vk)
            }, numeric(1))

            se_logSk <- sqrt(pmax(0, var_logSk))
            lcl <- exp(log(cum_est) - z * se_logSk)
            ucl <- exp(log(cum_est) + z * se_logSk)

            cum_phi <- data.frame(
              interval = phi_df$interval,
              estimate = cum_est,
              lcl = pmax(0, pmin(1, lcl)),
              ucl = pmax(0, pmin(1, ucl)),
              stringsAsFactors = FALSE
            )
            used_full_cov <- TRUE
          }
        }
      }
    }
  }, silent = TRUE)

  # Fallback: product of bounds (assumes independence)
  if (is.null(cum_phi)) {
    if (nrow(phi_df)) {
      cum_phi <- data.frame(
        interval = phi_df$interval,
        estimate = cumprod(pmax(0, pmin(1, phi_df$estimate))),
        lcl      = cumprod(pmax(0, pmin(1, phi_df$lcl))),
        ucl      = cumprod(pmax(0, pmin(1, phi_df$ucl))),
        stringsAsFactors = FALSE
      )
      warning("Used independence fallback for cumulative survival CIs (could not construct Phi covariance).",
              call. = FALSE)
    } else {
      cum_phi <- data.frame(interval = integer(), estimate = numeric(), lcl = numeric(), ucl = numeric())
    }
  }

  # ---- Plots ------------------------------------------------------------------
  make_ci_plot <- function(df, ylab, title) {
    if (!nrow(df)) return(NULL)
    if (requireNamespace("ggplot2", quietly = TRUE)) {
      ggplot2::ggplot(df, ggplot2::aes(x = interval, y = estimate)) +
        ggplot2::geom_point() +
        ggplot2::geom_errorbar(ggplot2::aes(ymin = lcl, ymax = ucl), width = 0.2) +
        ggplot2::ylim(0, 1) +
        ggplot2::labs(x = "Interval", y = ylab, title = title) +
        ggplot2::theme_minimal()
    } else {
      function() {
        op <- par(no.readonly = TRUE); on.exit(par(op))
        plot(df$interval, df$estimate, pch = 19, ylim = c(0,1),
             xlab = "Interval", ylab = ylab, main = title)
        segments(df$interval, df$lcl, df$interval, df$ucl)
        invisible(NULL)
      }
    }
  }

  plots <- list(
    phi     = make_ci_plot(phi_df,    "Survival (Phi)",      "Interval survival (Phi)"),
    p       = make_ci_plot(p_df,      "Detection (p)",       "Interval detection (p)"),
    cum_phi = make_ci_plot(cum_phi,   "Cumulative survival",
                           paste0("Cumulative survival to interval",
                                  if (used_full_cov) " (covariance-aware)" else " (independence fallback)"))
  )

  list(
    model   = mod,
    phi     = phi_df,
    cum_phi = cum_phi,
    p       = p_df,
    plots   = plots,
    covariance_mode = if (used_full_cov) "full" else "independence_fallback"
  )
}



#' Quick printer for fit_marked_cjs output
#' @param x object returned by fit_marked_cjs
#' @export
print.fit_marked_cjs <- function(x, ...) {
  cat("CJS model (marked::crm)\n")
  if (!is.null(x$model) && !is.null(x$model$model.name)) {
    cat("Model:", x$model$model.name, "\n")
  }
  cat("\nPhi (survival) estimates:\n")
  print(utils::head(x$phi, 10))
  cat("\n p (detection) estimates:\n")
  print(utils::head(x$p, 10))
  invisible(x)
}

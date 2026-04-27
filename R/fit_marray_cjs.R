#' @title Fit a CJS Model Using an M-array
#'
#' @description
#' Fits a CJS-style likelihood from a Burnham-style M-array. The final interval is
#' parameterized as \code{lambda_last = S_last * p_final}, because the final
#' survival and final detection probability are not separately identifiable.
#'
#' @param m_array Data frame from \code{build_marray()}.
#' @param conf_level Confidence level for Wald intervals.
#'
#' @return List with \code{model}, \code{phi}, \code{cum_phi}, \code{p},
#'   \code{lambda}, \code{estimates}, \code{plots}, and \code{covariance_mode}.
#'
#' @author Ryan N. Kinzer
#'
#' @export
fit_marray_cjs <- function(m_array,
                           conf_level = 0.95) {

  neg_logLik <- function(par, m_array) {

    count_cols <- c(grep("^m_", names(m_array), value = TRUE), "never")
    m_counts <- m_array[, count_cols, drop = FALSE]

    n_release <- nrow(m_counts)
    n_sites <- n_release + 1

    S <- rep(NA_real_, n_sites - 1)
    p <- rep(NA_real_, n_sites)

    S[1:(n_sites - 2)] <- stats::plogis(par[1:(n_sites - 2)])
    p[2:(n_sites - 1)] <- stats::plogis(par[(n_sites - 1):(2 * n_sites - 4)])
    lambda_last <- stats::plogis(par[2 * n_sites - 3])

    nll <- 0

    for (i in seq_len(n_release)) {

      later_sites <- (i + 1):n_sites

      probs <- numeric(length(later_sites) + 1)
      names(probs) <- c(paste0("m_", later_sites), "never")

      for (jj in seq_along(later_sites)) {

        j <- later_sites[jj]

        if (j < n_sites) {

          surv_prob <- prod(S[i:(j - 1)])

          miss_prob <- if (j > i + 1) {
            prod(1 - p[(i + 1):(j - 1)])
          } else {
            1
          }

          probs[jj] <- surv_prob * miss_prob * p[j]

        } else {

          surv_before_last <- if (i <= n_sites - 2) {
            prod(S[i:(n_sites - 2)])
          } else {
            1
          }

          miss_before_last <- if (n_sites > i + 1) {
            prod(1 - p[(i + 1):(n_sites - 1)])
          } else {
            1
          }

          probs[jj] <- surv_before_last * miss_before_last * lambda_last
        }
      }

      probs["never"] <- 1 - sum(probs[names(probs) != "never"])

      if (any(!is.finite(probs)) || any(probs < 0)) return(1e10)

      probs <- pmax(probs, 1e-12)
      counts <- as.numeric(m_counts[i, names(probs)])

      if (any(!is.finite(counts))) return(1e10)

      nll <- nll - sum(counts * log(probs))
    }

    nll
  }

  count_cols <- c(grep("^m_", names(m_array), value = TRUE), "never")
  n_sites <- nrow(m_array[, count_cols, drop = FALSE]) + 1
  n_par <- 2 * (n_sites - 2) + 1

  fit <- stats::optim(
    method = "BFGS",
    par = rep(0, n_par),
    fn = neg_logLik,
    m_array = m_array,
    hessian = TRUE
  )

  z <- stats::qnorm(1 - (1 - conf_level) / 2)

  vcov_beta <- tryCatch(
    solve(fit$hessian),
    error = function(e) NULL
  )

  se_beta <- if (is.null(vcov_beta)) {
    rep(NA_real_, length(fit$par))
  } else {
    sqrt(diag(vcov_beta))
  }

  beta <- fit$par

  estimates <- data.frame(
    parameter = c(
      paste0("S_", 1:(n_sites - 2)),
      paste0("p_", 2:(n_sites - 1)),
      paste0("lambda_", n_sites - 1)
    ),
    beta = beta,
    se_beta = se_beta,
    estimate = stats::plogis(beta),
    lcl = stats::plogis(beta - z * se_beta),
    ucl = stats::plogis(beta + z * se_beta),
    stringsAsFactors = FALSE
  )

  phi_df <- estimates[grepl("^S_", estimates$parameter), ]
  phi_df$interval <- seq_len(nrow(phi_df))
  phi_df <- phi_df[, c("interval", "estimate", "se_beta", "lcl", "ucl", "parameter")]
  names(phi_df)[names(phi_df) == "se_beta"] <- "se"

  p_df <- estimates[grepl("^p_", estimates$parameter), ]
  p_df$interval <- seq_len(nrow(p_df))
  p_df <- p_df[, c("interval", "estimate", "se_beta", "lcl", "ucl", "parameter")]
  names(p_df)[names(p_df) == "se_beta"] <- "se"

  lambda_df <- estimates[grepl("^lambda_", estimates$parameter), ]
  lambda_df$interval <- n_sites - 1
  lambda_df <- lambda_df[, c("interval", "estimate", "se_beta", "lcl", "ucl", "parameter")]
  names(lambda_df)[names(lambda_df) == "se_beta"] <- "se"

  vcov_phi <- if (!is.null(vcov_beta) && nrow(phi_df)) {
    vcov_beta[seq_len(nrow(phi_df)), seq_len(nrow(phi_df)), drop = FALSE]
  } else {
    NULL
  }

  cum <- compute_cum_survival(
    phi_df = phi_df,
    vcov_beta = vcov_phi,
    conf_level = conf_level
  )

  list(
    model = fit,
    phi = phi_df,
    cum_phi = cum$cum_phi,
    p = p_df,
    lambda = lambda_df,
    estimates = estimates,
    plots = list(),
    covariance_mode = cum$covariance_mode
  )
}

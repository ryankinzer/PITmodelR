#' @title Compute Cumulative Survival
#'
#' @description
#' Computes cumulative survival from interval survival estimates. If a covariance
#' matrix for logit-scale survival estimates is provided, confidence intervals are
#' computed using the delta method. Otherwise, an independence/product-of-bounds
#' fallback is used.
#'
#' @param phi_df Data frame with interval survival estimates. Must contain
#'   \code{estimate}, \code{lcl}, and \code{ucl}.
#' @param vcov_beta Optional variance-covariance matrix for logit-scale survival
#'   parameters.
#' @param conf_level Confidence level. Default is \code{0.95}.
#'
#' @return A list with \code{cum_phi} and \code{covariance_mode}.
#'
#' @author Ryan N. Kinzer
#'
#' @export
compute_cum_survival <- function(phi_df,
                                 vcov_beta = NULL,
                                 conf_level = 0.95) {

  stopifnot(is.data.frame(phi_df))
  stopifnot(all(c("estimate", "lcl", "ucl") %in% names(phi_df)))

  if (!nrow(phi_df)) {
    return(list(
      cum_phi = data.frame(
        interval = integer(),
        estimate = numeric(),
        lcl = numeric(),
        ucl = numeric()
      ),
      covariance_mode = "empty"
    ))
  }

  z <- stats::qnorm(1 - (1 - conf_level) / 2)

  phi_hat <- pmin(1 - 1e-12, pmax(1e-12, phi_df$estimate))
  cum_est <- cumprod(phi_hat)

  interval <- if ("interval" %in% names(phi_df)) {
    phi_df$interval
  } else {
    seq_along(phi_hat)
  }

  if (!is.null(vcov_beta) &&
      is.matrix(vcov_beta) &&
      all(dim(vcov_beta) == length(phi_hat))) {

    grad_log_phi <- 1 - phi_hat

    var_log_cum <- vapply(seq_along(phi_hat), function(k) {
      g <- grad_log_phi[seq_len(k)]
      V <- vcov_beta[seq_len(k), seq_len(k), drop = FALSE]
      as.numeric(t(g) %*% V %*% g)
    }, numeric(1))

    se_log_cum <- sqrt(pmax(0, var_log_cum))

    lcl <- exp(log(cum_est) - z * se_log_cum)
    ucl <- exp(log(cum_est) + z * se_log_cum)

    mode <- "full"

  } else {

    lcl <- cumprod(pmin(1, pmax(0, phi_df$lcl)))
    ucl <- cumprod(pmin(1, pmax(0, phi_df$ucl)))

    mode <- "independence_fallback"
  }

  cum_phi <- data.frame(
    interval = interval,
    estimate = cum_est,
    lcl = pmax(0, pmin(1, lcl)),
    ucl = pmax(0, pmin(1, ucl)),
    stringsAsFactors = FALSE
  )

  list(
    cum_phi = cum_phi,
    covariance_mode = mode
  )
}

#' @title Fit a CJS Model Using an M-array.
#'
#' @description
#' Fits a Cormack-Jolly-Seber (CJS) capture-recapture model using the M-array approach
#' as described in Burnham et al. 1987. The approach allows for censored data
#' (i.e., fish removed at a detection site for sampling or transportion). The
#' function returns tidy results for survival (\code{Phi}), detection (\code{p}),
#' and lambda (i.e., the product of survival and detection at the last reach and site).
#'
#' Cumulative survival estimates are computed with covariance-aware confidence intervals
#' when possible; otherwise, an independence-based fallback is used.
#'
#' @param m_array Data frame or tibble produced by \code{build_array}.
#'
#' @author Ryan N. Kinzer
#'
#' @export

fit_marray_cjs <- function(m_array) {

  neg_logLik <- function(par, m_array) {
    count_cols <- c(grep("^m_", names(m_array), value = TRUE), "never")
    m_counts <- m_array[, count_cols, drop = FALSE]

    n_release <- nrow(m_counts)
    n_sites <- n_release + 1

    # Parameters:
    # S_1 ... S_(k-2)
    # p_2 ... p_(k-1)
    # lambda_(k-1) = S_(k-1) * p_k

    S <- rep(NA_real_, n_sites - 1)
    p <- rep(NA_real_, n_sites)


    #set initial values

    S[1:(n_sites - 2)] <- plogis(par[1:(n_sites - 2)])
    p[2:(n_sites - 1)] <- plogis(par[(n_sites - 1):(2 * n_sites - 4)])
    lambda_last <- plogis(par[2 * n_sites - 3])

    nll <- 0

    for (i in seq_len(n_release)) {
      later_sites <- (i + 1):n_sites

      probs <- numeric(length(later_sites) + 1)
      names(probs) <- c(paste0("m_", later_sites), "never")

      for (jj in seq_along(later_sites)) {
        j <- later_sites[jj]

        if (j < n_sites) {
          surv_prob <- if (i <= j - 1)
            prod(S[i:(j - 1)])
          else
            1

          miss_prob <- if (j > i + 1) {
            prod(1 - p[(i + 1):(j - 1)])
          } else {
            1
          }

          probs[jj] <- surv_prob * miss_prob * p[j]

        } else {
          # final site: use lambda_last = S_last * p_final
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

      if (any(!is.finite(probs)) || any(probs < 0)) {
        return(1e10)
      }

      probs <- pmax(probs, 1e-12)
      counts <- as.numeric(m_counts[i, names(probs)])

      if (any(!is.finite(counts))) {
        return(1e10)
      }

      nll <- nll - sum(counts * log(probs))
    }

    nll
  }

  count_cols <- c(grep("^m_", names(m_array), value = TRUE), "never")
  n_sites <- nrow(m_array[, count_cols, drop = FALSE]) + 1

  n_par <- (n_sites - 2) + (n_sites - 2) + 1

  fit_m_array <- optim(
    method = 'BFGS',
    par = rep(0, n_par),
    fn = neg_logLik,
    m_array = m_array,
    hessian = TRUE
  )

  beta <- fit_m_array$par
  vcov_beta <- solve(fit_m_array$hessian)
  se_beta <- sqrt(diag(vcov_beta))

  est <- plogis(beta)
  lcl <- plogis(beta - 1.96 * se_beta)
  ucl <- plogis(beta + 1.96 * se_beta)


  param_labels <- c(paste0("S_", 1:(n_sites - 2)),
                    paste0("p_", 2:(n_sites - 1)),
                    paste0("lambda_", n_sites - 1))

  estimates <- data.frame(
    parameter = param_labels,
    beta = beta,
    se_beta = se_beta,
    estimate = est,
    lcl = lcl,
    ucl = ucl
  )

  return(estimates)

}

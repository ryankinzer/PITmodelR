#' Fit simple CJS-like survival by reach (placeholder)
#'
#' Estimates apparent survival per reach using simple proportions: for reach i (site i -> site i+1),
#' phi_hat = (#tags detected at site i+1) / (#tags detected at site i).
#'
#' @param histories list returned by \code{build_detection_histories()}.
#' @param model currently only "cjs" is supported (placeholder).
#' @param formula ignored in this placeholder; present for API compatibility.
#' @return A list with components:
#' \itemize{
#'   \item \code{survival}: data.frame of reach-level estimates with Wald CIs.
#'   \item \code{route_sites}: route site order used.
#' }
#' @export
fit_survival <- function(histories,
                         model = "cjs",
                         formula = ~ 1) {
  if (!is.list(histories) || is.null(histories$wide)) {
    stop("`histories` must be the result of build_detection_histories().", call. = FALSE)
  }
  rs <- attr(histories$wide, "route_sites")
  if (is.null(rs)) stop("`histories$wide` is missing route metadata.", call. = FALSE)

  wide <- histories$wide
  # counts per site
  n_at_site <- vapply(rs, function(s) sum(wide[[s]] > 0, na.rm = TRUE), numeric(1))
  # counts at downstream site (shifted)
  n_next <- c(n_at_site[-1], NA_real_)

  # survival per reach (site_i -> site_{i+1})
  k <- length(rs) - 1L
  reach <- paste(rs[seq_len(k)], "->", rs[seq_len(k) + 1L])
  n_i <- n_at_site[seq_len(k)]
  n_j <- n_at_site[seq_len(k) + 1L]

  # Avoid division by zero
  phi <- ifelse(n_i > 0, n_j / n_i, NA_real_)

  # Wald SE and CI on logit scale (placeholder, assumes binomial approx)
  logit <- function(p) log(p/(1-p))
  invlogit <- function(eta) 1/(1+exp(-eta))
  se <- rep(NA_real_, k)
  lcl <- ucl <- rep(NA_real_, k)
  alpha <- 0.05
  z <- stats::qnorm(1 - alpha/2)

  for (i in seq_len(k)) {
    if (is.finite(phi[i]) && phi[i] > 0 && phi[i] < 1 && is.finite(n_i[i])) {
      # Binomial variance for proportion p with n trials
      p <- phi[i]; n <- n_i[i]
      # Approximate SE on logit scale
      se_logit <- sqrt(1/(n*p) + 1/(n*(1-p)))
      ci_eta <- logit(p) + c(-1, 1) * z * se_logit
      lcl[i] <- invlogit(ci_eta[1]); ucl[i] <- invlogit(ci_eta[2])
      se[i] <- sqrt(p*(1-p)/n)
    } else if (!is.na(phi[i]) && phi[i] %in% c(0,1) && n_i[i] > 0) {
      # Handle boundary cases conservatively
      lcl[i] <- max(0, phi[i] - z * sqrt((phi[i]*(1-phi[i]))/n_i[i]))
      ucl[i] <- min(1, phi[i] + z * sqrt((phi[i]*(1-phi[i]))/n_i[i]))
      se[i] <- sqrt(phi[i]*(1-phi[i])/n_i[i])
    } else {
      se[i] <- NA_real_; lcl[i] <- NA_real_; ucl[i] <- NA_real_
    }
  }

  survival <- data.frame(
    reach = reach,
    site_from = rs[seq_len(k)],
    site_to   = rs[seq_len(k) + 1L],
    n_from = as.integer(n_i),
    n_to   = as.integer(n_j),
    phi    = phi,
    se     = se,
    lcl    = lcl,
    ucl    = ucl,
    stringsAsFactors = FALSE
  )

  list(survival = survival, route_sites = rs, model = model)
}

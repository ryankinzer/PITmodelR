#' @title Summarize Reach-Level Survival Estimates
#'
#' @description
#' Extracts interval survival estimates and confidence intervals from a fitted
#' survival object returned by \code{fit_survival()}.
#'
#' @param fit List returned by \code{fit_survival()}.
#' @param level Numeric; confidence level for intervals (currently informational;
#'   intervals are already computed inside \code{fit_survival()}).
#'
#' @return A data.frame of reach-level survival estimates with columns:
#' \describe{
#'   \item{\code{reach}}{Reach or interval identifier.}
#'   \item{\code{phi}}{Apparent survival estimate for the reach.}
#'   \item{\code{lcl}}{Lower confidence limit.}
#'   \item{\code{ucl}}{Upper confidence limit.}
#' }
#'
#' @author Ryan Kinzer
#'
#' @export
summarize_survival <- function(fit, level = 0.95) {

  if (!is.list(fit) || is.null(fit$survival)) {
    stop("`fit` must be the result of fit_survival().", call. = FALSE)
  }

  fit$survival

}

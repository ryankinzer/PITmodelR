#' Summarize survival fit (placeholder)
#' @param fit object returned by \code{fit_survival()}.
#' @param level confidence level for intervals (currently informational; CIs computed inside fit).
#' @return data.frame with reach-level survival and CIs.
#' @export
summarize_survival <- function(fit, level = 0.95) {
  if (!is.list(fit) || is.null(fit$survival)) {
    stop("`fit` must be the result of fit_survival().", call. = FALSE)
  }
  fit$survival
}

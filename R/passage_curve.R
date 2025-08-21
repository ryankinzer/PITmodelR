#' Generate cumulative passage curve data
#' @param timing list returned by \code{fit_timing()}.
#' @return data.frame with date and cumulative proportion.
#' @export
passage_curve <- function(timing) {
  if (!is.list(timing) || is.null(timing$daily)) {
    stop("`timing` must be the result of fit_timing().", call. = FALSE)
  }
  timing$daily[, c("date", "prop")]
}

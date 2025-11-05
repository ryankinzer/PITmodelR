#' @title Generate Cumulative Passage Curve Data
#'
#' @description
#' Extracts the cumulative passage curve from the output of \code{fit_timing()}.
#' The curve consists of the aggregation date and cumulative proportion of total
#' observations up to that date.
#'
#' @param timing A list returned by \code{fit_timing()} containing a
#'   \code{daily} component with date-level passage information.
#'
#' @return A data frame with two columns:
#' \describe{
#'   \item{\code{date}}{Aggregation date (typically daily).}
#'   \item{\code{prop}}{Cumulative proportion of total events through that date.}
#' }
#'
#' @export

passage_curve <- function(timing) {

  if (!is.list(timing) || is.null(timing$daily)) {
    stop("`timing` must be the result of fit_timing().", call. = FALSE)
  }
  timing$daily[, c("date", "prop")]

}

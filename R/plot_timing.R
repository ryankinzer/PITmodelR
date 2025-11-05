#' @title Plot Cumulative Passage Over Time
#'
#' @description
#' Creates a base R line plot of cumulative passage through a site over time.
#' Accepts either a list returned by \code{fit_timing()} or a data frame produced
#' by \code{passage_curve()}.
#'
#' @param timing A list from \code{fit_timing()} or a data frame from
#'   \code{passage_curve()} containing at minimum columns \code{date} and \code{prop}.
#'
#' @return Invisibly returns \code{NULL}.
#'
#' @author Ryan Kinzer
#'
#' @export

plot_timing <- function(timing) {

  pc <- if (is.list(timing) && !is.null(timing$daily)) passage_curve(timing) else timing

  if (!is.data.frame(pc) || !all(c("date","prop") %in% names(pc))) {
    stop("`timing` must be a list from fit_timing() or a data.frame with columns 'date' and 'prop'.", call. = FALSE)
  }

  plot(pc$date, pc$prop, type = "l", xlab = "Date", ylab = "Cumulative passage", ylim = c(0,1))
  invisible(NULL)

}

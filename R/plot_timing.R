#' Plot cumulative passage over time (base graphics placeholder)
#' @param timing list from \code{fit_timing()} or data.frame from \code{passage_curve()}.
#' @export
plot_timing <- function(timing) {
  pc <- if (is.list(timing) && !is.null(timing$daily)) passage_curve(timing) else timing
  if (!is.data.frame(pc) || !all(c("date","prop") %in% names(pc))) {
    stop("`timing` must be a list from fit_timing() or a data.frame with columns 'date' and 'prop'.", call. = FALSE)
  }
  plot(pc$date, pc$prop, type = "l", xlab = "Date", ylab = "Cumulative passage", ylim = c(0,1))
  invisible(NULL)
}

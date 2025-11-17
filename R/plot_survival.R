#' @title Plot Apparent Survival by Reach
#'
#' @description
#' Creates a base R plot of apparent survival estimates (\code{phi}) by reach.
#' Error bars are drawn if lower and upper confidence limits (\code{lcl}, \code{ucl})
#' are available in the input table.
#'
#' @param surv_tbl A data frame returned by \code{summarize_survival()}, containing
#'   at minimum the columns \code{reach} and \code{phi}. Optional columns
#'   \code{lcl} and \code{ucl} are used for error bars.
#'
#' @return Invisibly returns \code{NULL}.
#'
#' @author Ryan Kinzer
#'
#' @export

plot_survival <- function(surv_tbl) {

  if (!is.data.frame(surv_tbl) || !all(c("reach","phi") %in% names(surv_tbl))) {
    stop("`surv_tbl` must contain columns 'reach' and 'phi'.", call. = FALSE)
  }

  op <- graphics::par(no.readonly = TRUE); on.exit(graphics::par(op))
  graphics::par(mar = c(8, 4, 2, 1))
  x <- seq_len(nrow(surv_tbl))
  plot(x, surv_tbl$phi, xaxt = "n", xlab = "", ylab = "Apparent survival (phi)",
       ylim = c(0, 1), pch = 19)
  graphics::axis(1, at = x, labels = surv_tbl$reach, las = 2, cex.axis = 0.8)

  # error bars if available
  if (all(c("lcl","ucl") %in% names(surv_tbl))) {
    graphics::segments(x0 = x, y0 = surv_tbl$lcl, x1 = x, y1 = surv_tbl$ucl)
  }

  invisible(NULL)
}

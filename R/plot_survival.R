#' Plot survival by reach (base graphics placeholder)
#' @param surv_tbl table returned by \code{summarize_survival()}.
#' @export
plot_survival <- function(surv_tbl) {
  if (!is.data.frame(surv_tbl) || !all(c("reach","phi") %in% names(surv_tbl))) {
    stop("`surv_tbl` must contain columns 'reach' and 'phi'.", call. = FALSE)
  }
  op <- par(no.readonly = TRUE); on.exit(par(op))
  par(mar = c(8, 4, 2, 1))
  x <- seq_len(nrow(surv_tbl))
  plot(x, surv_tbl$phi, xaxt = "n", xlab = "", ylab = "Apparent survival (phi)",
       ylim = c(0, 1), pch = 19)
  axis(1, at = x, labels = surv_tbl$reach, las = 2, cex.axis = 0.8)
  # error bars if available
  if (all(c("lcl","ucl") %in% names(surv_tbl))) {
    segments(x0 = x, y0 = surv_tbl$lcl, x1 = x, y1 = surv_tbl$ucl)
  }
  invisible(NULL)
}

#' Quick printer for fit_marked_cjs output
#'
#' @param x object returned by fit_marked_cjs
#' @param ... additional arguments (unused)
#'
#' @export

print_fit_marked_cjs <- function(x, ...) {

  cat("CJS model (marked::crm)\n")
  if (!is.null(x$model) && !is.null(x$model$model.name)) {
    cat("Model:", x$model$model.name, "\n")
  }
  cat("\nPhi (survival) estimates:\n")
  print(utils::head(x$phi, 10))
  cat("\n p (detection) estimates:\n")
  print(utils::head(x$p, 10))
  invisible(x)

}

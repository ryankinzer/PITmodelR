#' @title Print CJS Model Summary
#'
#' @description
#' Provides a concise display of results from \code{fit_marked_cjs()}, including
#' the model name, the first several survival (\code{phi}) estimates, and the
#' first several detection (\code{p}) estimates.
#'
#' @param x An object returned by \code{fit_marked_cjs()}.
#' @param ... Additional arguments (currently unused).
#'
#' @return Invisibly returns the input object \code{x}.
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

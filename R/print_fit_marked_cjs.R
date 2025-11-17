#' @title Print CJS Model Summary
#'
#' @description
#' Provides a concise display of results from \code{fit_marked_cjs()}, including
#' the model name, the first several survival (\code{phi}) estimates, and the
#' first several detection (\code{p}) estimates.
#'
#' @param z An object returned by \code{fit_marked_cjs()}.
#' @param ... Additional arguments (currently unused).
#'
#' @return Invisibly returns the input object \code{z}.
#'
#' @export

print_fit_marked_cjs <- function(z, ...) {

  cat("CJS model (marked::crm)\n")
  if (!is.null(z$model) && !is.null(z$model$model.name)) {
    cat("Model:", z$model$model.name, "\n")
  }
  cat("\nPhi (survival) estimates:\n")
  print(utils::head(z$phi, 10))
  cat("\n p (detection) estimates:\n")
  print(utils::head(z$p, 10))
  invisible(z)

}

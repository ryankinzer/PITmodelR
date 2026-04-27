#' @title Fit a CJS Model Using `marked`
#'
#' @description
#' Fits a Cormack-Jolly-Seber model using \code{marked} and returns tidy survival,
#' cumulative survival, and detection estimates.
#'
#' @param ch_data Data frame with \code{tag_code}, \code{ch}, and optional
#'   individual covariates.
#' @param phi_formula Formula for survival. Default \code{~ time}.
#' @param p_formula Formula for detection. Default \code{~ time}.
#' @param hessian Logical; passed to \code{marked::crm()}.
#' @param conf_level Confidence level for intervals.
#' @param ... Additional arguments passed to \code{marked::crm()}.
#'
#' @return List with \code{model}, \code{phi}, \code{cum_phi}, \code{p},
#'   \code{plots}, and \code{covariance_mode}.
#'
#' @author Ryan N. Kinzer
#'
#' @export
fit_marked_cjs <- function(ch_data,
                           phi_formula = ~ time,
                           p_formula   = ~ time,
                           hessian     = TRUE,
                           conf_level  = 0.95,
                           ...) {

  if (!requireNamespace("marked", quietly = TRUE)) {
    stop("Package 'marked' must be installed.", call. = FALSE)
  }

  stopifnot(is.data.frame(ch_data), all(c("tag_code", "ch") %in% names(ch_data)))

  proc <- marked::process.data(ch_data, model = "CJS")
  ddl  <- marked::make.design.data(proc)

  mod <- marked::crm(
    proc,
    ddl,
    model.parameters = list(
      Phi = list(formula = phi_formula),
      p   = list(formula = p_formula)
    ),
    hessian = hessian,
    ...
  )

  pred_phi <- marked::predict.crm(mod, parameter = "Phi")$Phi
  pred_p   <- marked::predict.crm(mod, parameter = "p")$p

  tidy_pred <- function(x) {
    data.frame(
      interval = seq_len(nrow(x)),
      estimate = x$estimate,
      se = x$se,
      lcl = x$lcl,
      ucl = x$ucl,
      stringsAsFactors = FALSE
    )
  }

  phi_df <- tidy_pred(pred_phi)
  p_df   <- tidy_pred(pred_p)

  cum <- compute_cum_survival(
    phi_df = phi_df,
    vcov_beta = NULL,
    conf_level = conf_level
  )

  list(
    model = mod,
    phi = phi_df,
    cum_phi = cum$cum_phi,
    p = p_df,
    plots = list(),
    covariance_mode = cum$covariance_mode,
    raw = list(Phi = pred_phi, p = pred_p)
  )
}

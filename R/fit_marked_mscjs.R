#' @title Fit a Multistate CJS Model Using `marked`
#'
#' @description
#' Fits a multistate CJS model using \code{marked::crm()} with
#' \code{model = "hmmMSCJS"}. Designed for encounter histories where censored or
#' removed fish are represented as a separate absorbing state.
#'
#' @param ms_data Data frame from \code{build_multistate_histories()} with
#'   \code{tag_code}, \code{ch}, and optional covariates.
#' @param s_formula Formula for survival. Default \code{~ time}.
#' @param p_formula Formula for detection. Default \code{~ time}.
#' @param psi_formula Formula for transition probabilities. Default
#'   \code{~ -1 + stratum:tostratum}.
#' @param hessian Logical; passed to \code{marked::crm()}.
#' @param conf_level Confidence level for intervals.
#' @param ... Additional arguments passed to \code{marked::crm()}.
#'
#' @return List with \code{model}, \code{phi}, \code{cum_phi}, \code{p},
#'   \code{psi}, \code{plots}, and \code{covariance_mode}.
#'
#' @author Ryan N. Kinzer
#'
#' @export
fit_marked_mscjs <- function(ms_data,
                             s_formula   = ~ time,
                             p_formula   = ~ time,
                             psi_formula = ~ -1 + stratum:tostratum,
                             hessian     = TRUE,
                             conf_level  = 0.95,
                             ...) {

  if (!requireNamespace("marked", quietly = TRUE)) {
    stop("Package 'marked' must be installed.", call. = FALSE)
  }

  stopifnot(is.data.frame(ms_data), all(c("tag_code", "ch") %in% names(ms_data)))

  proc <- marked::process.data(
    ms_data,
    model = "hmmMSCJS",
    strata.labels = c("A", "C")
  )

  ddl <- marked::make.design.data(proc)

  ddl$p$fix <- NA
  ddl$p$fix[ddl$p$stratum == "C"] <- 0

  ddl$Psi$fix <- NA
  ddl$Psi$fix[ddl$Psi$stratum == "C" & ddl$Psi$tostratum == "A"] <- 0
  ddl$Psi$fix[ddl$Psi$stratum == "C" & ddl$Psi$tostratum == "C"] <- 1

  mod <- marked::crm(
    proc,
    ddl,
    model = "hmmMSCJS",
    model.parameters = list(
      S   = list(formula = s_formula),
      p   = list(formula = p_formula),
      Psi = list(formula = psi_formula)
    ),
    hessian = hessian,
    ...
  )

  pred_s   <- marked::predict.crm(mod, parameter = "S")$S
  pred_p   <- marked::predict.crm(mod, parameter = "p")$p
  pred_psi <- marked::predict.crm(mod, parameter = "Psi")$Psi

  phi_df <- data.frame(
    interval = seq_len(nrow(pred_s)),
    estimate = pred_s$estimate,
    se = pred_s$se,
    lcl = pred_s$lcl,
    ucl = pred_s$ucl,
    stringsAsFactors = FALSE
  )

  p_use <- pred_p
  if ("stratum" %in% names(p_use)) {
    p_use <- p_use[p_use$stratum == "A", , drop = FALSE]
  } else {
    p_use <- p_use[p_use$estimate > 0, , drop = FALSE]
  }

  p_df <- data.frame(
    interval = seq_len(nrow(p_use)),
    estimate = p_use$estimate,
    se = p_use$se,
    lcl = p_use$lcl,
    ucl = p_use$ucl,
    stringsAsFactors = FALSE
  )

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
    psi = pred_psi,
    plots = list(),
    covariance_mode = cum$covariance_mode,
    raw = list(S = pred_s, p = pred_p, Psi = pred_psi)
  )
}

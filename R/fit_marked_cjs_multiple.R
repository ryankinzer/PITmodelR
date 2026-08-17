#' @title Fit a CJS Model Using `marked`
#'
#' @description
#' Fits a Cormack-Jolly-Seber model for multiple release groups using \code{marked} and returns survival,
#' cumulative survival, and detection estimates. Detection probability is estimated jointly for all release groups
#' and survival is estimated independently.
#'
#' @param ch_data Data frame with \code{tag_code}, \code{ch}, \code{release_group} and optional
#'   individual covariates.
#' @param site_mapping Data frame with \code{release_group}, \code{detect_site}, and \code{occ}.
#' @param phi_formula Formula for survival. Default \code{~ time*release_group}.
#' @param p_formula Formula for detection. Default \code{~ time*detect_site}.
#' @param hessian Logical; passed to \code{marked::crm()}.
#' @param conf_level Confidence level for intervals.
#' @param ... Additional arguments passed to \code{marked::crm()}.
#'
#' @return List with \code{model}, \code{phi}, \code{cum_phi}, \code{p},
#'   \code{plots}, and \code{covariance_mode}.
#'
#' @author Ryan N. Kinzer
#' @authur Michelle A. Briggs
#'
#' @export
fit_marked_cjs_multiple <- function(ch_data,
                           site_mapping,
                           phi_formula = ~ time*release_group,
                           p_formula   = ~ time*detect_site,
                           hessian     = TRUE,
                           conf_level  = 0.95,
                           ...) {

  if (!requireNamespace("marked", quietly = TRUE)) {
    stop("Package 'marked' must be installed.", call. = FALSE)
  }

  stopifnot(is.data.frame(ch_data), all(c("tag_code", "ch") %in% names(ch_data)))

  proc <- marked::process.data(ch_data, model = "CJS", groups = "release_group")
  ddl  <- marked::make.design.data(proc)

  ddl$p <- dplyr::left_join(ddl$p, site_mapping, by = c("release_group", "occ"))
  ddl$p <- ddl$p %>%
    dplyr::mutate(detect_site = if_else(is.na(detect_site), release_group, detect_site))

  ddl$p$detect_site <- as.factor(ddl$p$detect_site)

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

  pred <- predict(mod)

  pred_phi <- pred$Phi
  pred_p   <- pred$p

  # pred_phi <- marked::predict.crm(mod, parameter = "Phi")$Phi
  # pred_p   <- marked::predict.crm(mod, parameter = "p")$p

  tidy_pred_phi <- function(x) {
    data.frame(
      interval = x$time,
      release_group = x$release_group,
      estimate = x$estimate,
      se = x$se,
      lcl = x$lcl,
      ucl = x$ucl,
      stringsAsFactors = FALSE
    )
  }

  tidy_pred_p <- function(x) {
    data.frame(
      interval = x$time,
      detect_site = x$detect_site,
      estimate = x$estimate,
      se = x$se,
      lcl = x$lcl,
      ucl = x$ucl,
      stringsAsFactors = FALSE
    )
  }

  phi_df <- tidy_pred_phi(pred_phi)
  p_df   <- tidy_pred_p(pred_p)

  cum <- phi_df %>%
    split(.$release_group) %>%
    purrr::map(~compute_cum_survival(phi_df = .x,
                              vcov_beta = NULL,
                              conf_level = conf_level)) %>%
    purrr::map(~ dplyr::mutate(.x$cum_phi)) %>%
    list_rbind(names_to = "release_group")


  list(
    model = mod,
    phi = phi_df,
    cum_phi = cum,
    p = p_df,
    plots = list(),
    covariance_mode = cum$covariance_mode,
    raw = list(Phi = pred_phi, p = pred_p)
  )
}

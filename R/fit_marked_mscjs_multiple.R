@title Fit a Multistate CJS Model Using `marked`
#'
#' @description
#' Fits a multistate CJS model for multiple release groups using \code{marked::crm()} with
#' \code{model = "hmmMSCJS"}. Designed for encounter histories where censored or
#' removed fish are represented as a separate absorbing state. Survival is estimated separately
#' for each release group, and detection probability is estimated jointly.
#'
#' @param ms_data Data frame from \code{build_multistate_histories()} with
#'   \code{tag_code}, \code{ch}, and optional covariates.
#' @param site_mapping Data frame with \code{release_group}, \code{detect_site}, and \code{occ}.
#' @param s_formula Formula for survival. Default \code{~ time}.
#' @param p_formula Formula for detection. Default \code{~ time}.
#' @param psi_formula Formula for transition probabilities. Default
#'   \code{~ -1 + stratum:tostratum}.
#' @param hessian Logical; passed to \code{marked::crm()}.
#' @param conf_level Confidence level for intervals.
#' @param ... Additional arguments passed to \code{marked::crm()}.
#'
#' @return List with \code{model}, \code{phi}, \code{cum_phi}, \code{p},
#'   \code{psi}, and \code{covariance_mode}.
#'
#' @author Ryan N. Kinzer
#' @author Michelle A. Briggs
#'
#' @export

fit_marked_mscjs_multiple <- function(ms_data,
                                      site_mapping,
                                      s_formula   = ~ time*release_group,
                                      p_formula   = ~ stratum*time*detect_site,
                                      psi_formula = ~ -1 + stratum:tostratum,
                                      hessian     = TRUE,
                                      conf_level  = 0.95,
                                      ...) {

  if (!requireNamespace("marked", quietly = TRUE)) {
    stop("Package 'marked' must be installed.", call. = FALSE)
  }

  stopifnot(is.data.frame(ms_data), all(c("tag_code", "ch") %in% names(ms_data)))

  proc <- marked::process.data(
    marked_ch,
    model = "hmmMSCJS",
    groups = "release_group",
    strata.labels = c("A", "C")
  )

  ddl <- marked::make.design.data(proc)

  ddl$p <- dplyr::left_join(ddl$p, site_mapping, by = c("release_group", "occ"))
  ddl$p <- ddl$p %>%
    dplyr::mutate(detect_site = if_else(is.na(detect_site), release_group, detect_site))

  ddl$p$detect_site <- as.factor(ddl$p$detect_site)

  ddl$p$fix <- NA
  ddl$p$fix[ddl$p$stratum == "C" & ddl$p$occ == max(ddl$p$occ)] <- 1

  #fix p to 0 for stratum == C for detections above Down, because censoring doesn't occur
  #model runs slightly faster
  #check that this assumption holds

  ddl$p$fix[ddl$p$stratum == "C" & ddl$p$occ < max(ddl$p$occ)] <- 0

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

  pred <- predict(mod)

  pred_s   <- pred$S
  pred_p   <- pred$p
  pred_psi <- pred$Psi

  # pred_s   <- marked::predict.crm(mod, parameter = "S")$S
  # pred_p   <- marked::predict.crm(mod, parameter = "p")$p
  # pred_psi <- marked::predict.crm(mod, parameter = "Psi")$Psi

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

  p_use <- pred_p
  if ("stratum" %in% names(p_use)) {
    p_use <- p_use[p_use$stratum == "A", , drop = FALSE]
  } else {
    p_use <- p_use[p_use$estimate > 0, , drop = FALSE]
  }


  phi_df <- tidy_pred_phi(pred_s)
  p_df   <- tidy_pred_p(p_use)

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
    psi = pred_psi,
    plots = list(),
    covariance_mode = cum$covariance_mode,
    raw = list(S = pred_s, p = pred_p, Psi = pred_psi)
  )
}

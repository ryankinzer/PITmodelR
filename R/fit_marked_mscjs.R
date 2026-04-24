#' @title Fit a Multi-state CJS Model Using `marked`
#'
#' @description
#' Fits a Cormack-Jolly-Seber (CJS) capture-recapture model with right censored
#' observations using the `marked` package and the model \code{hmmMSCJS}. The function
#' returns tidy results for survival (\code{Phi}) and detection (\code{p}) parameters.
#' Cumulative survival estimates are computed with covariance-aware confidence intervals
#' when possible; otherwise, an independence-based fallback is used.
#'
#' @param ms_data Data frame or tibble with at least the following columns:
#'   \describe{
#'     \item{\code{tag_code}}{Unique identifier for each tagged individual.}
#'     \item{\code{ch}}{Encounter history string (e.g., "AA0A0CC"). The value A
#'     equal alive and detected, 0 equal not detected, and C equals censored or
#'     removed after being detected.}
#'   }
#'
#' @return a data frame
#'
#' @details
#' This function wraps the `marked::crm` and sets the model fit to \code{hmmMSCJS}.
#'
#' @seealso [marked::crm]
#'
#' @author Ryan N. Kinzer
#'
#' @export

fit_marked_mscjs <- function(ms_data) {

  proc <- marked::process.data(
    ms_data,
    model = "hmmMSCJS",
    strata.labels = c("A", "C")
  )

  ddl <- marked::make.design.data(proc)

  # Censored fish cannot be detected
  ddl$p$fix <- NA
  ddl$p$fix[ddl$p$stratum == "C"] <- 0

  ddl$Psi$fix <- NA
  ddl$Psi$fix[ddl$Psi$stratum == "C" & ddl$Psi$tostratum == "A"] <- 0
  ddl$Psi$fix[ddl$Psi$stratum == "C" & ddl$Psi$tostratum == "C"] <- 1

  fit <- crm(
    proc,
    ddl,
    model = "hmmMSCJS",
    model.parameters = list(
      S   = list(formula = ~ time),
      p   = list(formula = ~ time),
      Psi = list(formula = ~ -1 + stratum:tostratum)
    ),
    hessian = TRUE
  )

  return(fit)

  # summary(mod1)
  #
  # predict(mod1, parameter = "S")
  # predict(mod1, parameter = "p")
  # predict(mod1, parameter = "Psi")
}

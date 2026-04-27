#' @title Build Multistate Histories for `marked`
#'
#' @description
#' Converts encounter histories coded as \code{0}, \code{1}, and \code{2} to
#' multistate histories for \code{marked::crm(..., model = "hmmMSCJS")}.
#'
#' @details
#' Input coding:
#' \itemize{
#'   \item \code{1}: detected and released
#'   \item \code{2}: detected and then censored/removed
#'   \item \code{0}: not detected
#' }
#'
#' Output coding:
#' \itemize{
#'   \item \code{A}: alive/available and detected
#'   \item \code{C}: censored/removed and unavailable
#'   \item \code{0}: not detected
#' }
#'
#' Because removal occurs after detection, a \code{2} is coded as \code{A} at
#' that occasion and \code{C} for all subsequent occasions.
#'
#' @param ch_data Data frame with at least \code{tag_code} and \code{ch}.
#' @param tag_col Character; tag column name. Default \code{"tag_code"}.
#' @param ch_col Character; encounter history column name. Default \code{"ch"}.
#' @param keep_cols Logical; if \code{TRUE}, retain additional columns such as
#'   group covariates. Default \code{TRUE}.
#'
#' @return Data frame with \code{tag_code}, converted \code{ch}, and optionally
#'   additional covariates.
#'
#' @author Ryan N. Kinzer
#'
#' @export
build_multistate_histories <- function(ch_data,
                                       tag_col = "tag_code",
                                       ch_col = "ch",
                                       keep_cols = TRUE) {

  stopifnot(is.data.frame(ch_data), all(c(tag_col, ch_col) %in% names(ch_data)))

  convert_ch <- function(ch) {

    x <- strsplit(as.character(ch), "")[[1]]
    censor_pos <- which(x == "2")[1]

    x[x == "1"] <- "A"
    x[x == "0"] <- "0"

    if (!is.na(censor_pos)) {
      x[censor_pos] <- "A"

      if (censor_pos < length(x)) {
        x[(censor_pos + 1):length(x)] <- "C"
      }
    }

    paste0(x, collapse = "")
  }

  out <- ch_data

  out[[ch_col]] <- vapply(out[[ch_col]], convert_ch, character(1))

  names(out)[names(out) == tag_col] <- "tag_code"
  names(out)[names(out) == ch_col] <- "ch"

  if (!keep_cols) {
    out <- out[, c("tag_code", "ch"), drop = FALSE]
  }

  out
}

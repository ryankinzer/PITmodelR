#' @title Build capture histories for a multi-state model using `marked`
#'
#' @description
#' Converts capture histories from \code{build_capture_histories} to a format
#' usable for multi-state modeling in package \code{marked}.
#'
#' @param ch_data Data frame or tibble with at least the following columns:
#'   \describe{
#'     \item{\code{tag_code}}{Unique identifier for each tagged individual.}
#'     \item{\code{ch}}{Encounter history string (e.g., "11001"). The value
#'     1 = released/detected, 0 = not observed, and a 2 = detected but removed and not available for observation at the next site.}
#'   }
#'
#' @author Ryan N. Kinzer
#'
#' @export
build_multistate_histories <- function(ch_data) {

  convert_ch <- function(ch) {

    x <- strsplit(ch, "")[[1]]

    censor_pos <- which(x == "2")[1]

    if (is.na(censor_pos)) {

      x[x == "1"] <- "A"
      x[x == "0"] <- "0"

    } else {

      # before censoring
      if (censor_pos > 1) {
        x[1:(censor_pos - 1)][x[1:(censor_pos - 1)] == "1"] <- "A"
        x[1:(censor_pos - 1)][x[1:(censor_pos - 1)] == "0"] <- "0"
      }

      # censoring occasion: detected while still available
      x[censor_pos] <- "A"

      # after censoring: removed/unavailable state
      if (censor_pos < length(x)) {
        x[(censor_pos + 1):length(x)] <- "C"
      }
    }

    paste0(x, collapse = "")
  }

  out <- data.frame(
    tag_code = ch_data$tag_code,
    ch = vapply(ch_data$ch, convert_ch, character(1)),
    stringsAsFactors = FALSE
  )

  out
}

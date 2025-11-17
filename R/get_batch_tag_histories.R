#' @title Download and Combine Multiple Tag Histories
#'
#' @description
#' Retrieves PIT tag observation histories for a set of tag codes and combines them
#' into a single tibble. Each tag’s history is downloaded individually using
#' \code{get_tag_history()}, and a \code{tag_code} column is added to identify
#' the originating tag.
#'
#' @param tag_codes Character vector of PIT tag codes to retrieve.
#'
#' @inheritParams get_tag_history
#'
#' @return A tibble containing the combined tag histories for all requested tag codes,
#'   including an added \code{tag_code} column.
#'
#' @author Ryan Kinzer
#'
#' @examples
#' \dontrun{
#' get_batch_tag_histories(c("384.1B79726A98", "384.1B79726B01"))
#' }
#'
#' @export

get_batch_tag_histories <- function(tag_codes, api_key = NULL, fields = NULL) {

  if (!is.character(tag_codes) || !length(tag_codes)) {
    stop("`tag_codes` must be a non-empty character vector.", call. = FALSE)
  }

  # iterate over tag codes
  res_list <- lapply(tag_codes, function(tc) {
    df <- tryCatch(
      get_tag_history(api_key = api_key, tag_code = tc, fields = fields),
      error = function(e) {
        warning("Failed to download tag ", tc, ": ", conditionMessage(e))
        tibble::tibble()
      }
    )
    if (nrow(df) > 0) {
      df$tag_code <- tc
    }
    df
  })

  # bind all into one tibble
  out <- dplyr::bind_rows(res_list)

  # format event_date
  if ("event_date" %in% names(out)) {
    # try to parse full datetime with time zone (UTC safest for PTAGIS API)
    dt <- as.POSIXct(out$event_date, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")

    # if parsing fails, fallback to Date
    if (all(is.na(dt))) {
      out$event_date <- as.Date(out$event_date, format = "%Y-%m-%d")
    } else {
      out$event_date <- dt
    }
  }

  tibble::as_tibble(out)
}

#' @title Download and parse a single PTAGIS MRR file
#'
#' @description
#' Downloads a single PTAGIS mark–recapture–recovery (MRR) file and parses it
#' into standardized tibbles. Parsing is file-type specific (JSON, XML, or TXT),
#' while schema enforcement and PDV/SPDV capture are centralized in
#' \code{\link{enforce_schema}}.
#'
#' @param filename Character scalar. Name of the MRR file to download
#'   (e.g., \code{"CDR-2024-072-JCT.json"}, \code{"NPC-2019-073-001.xml"}, \code{"CDR06078.JCT"}).
#' @param drop_pdvs Logical, default \code{FALSE}. If \code{TRUE}, drops PDV-related
#'   components from the returned object (\code{session_pdv_fields},
#'   \code{detail_pdv_fields}, and \code{pdv_values}). The standardized \code{session}
#'   and \code{events} outputs are always returned.
#'
#' @return A named list with at least:
#' \itemize{
#'   \item \code{session}: a one-row tibble of session metadata (standardized schema)
#'   \item \code{events}: a tibble of event records (standardized schema)
#' }
#' If \code{drop_pdvs = FALSE}, also includes:
#' \itemize{
#'   \item \code{session_pdv_fields}: tibble mapping session SPDVs (pdv_column, label_raw, label, definition)
#'   \item \code{detail_pdv_fields}: tibble mapping event PDVs (pdv_column, label_raw, label, definition)
#'   \item \code{pdv_values}: list with \code{session} and \code{events} long-form PDV value tables
#' }
#'
#' @details
#' The function determines file type from \code{filename}'s extension:
#' JSON (P5), XML (P4), or legacy TXT/ASCII (P3). If a file appears to be XML/JSON
#' but lacks the expected extension, an error is raised.
#'
#' @author Mike Ackerman & Ryan Kinzer
#'
#' @export
get_file_data <- function(filename,
                          drop_pdvs = FALSE) {

  # --- validate filename ---
  if (missing(filename) ||
      !is.character(filename) ||
      length(filename) != 1L ||
      !nzchar(filename)) {
    stop(
      "`filename` must be a single non-empty string (e.g., 'CDR-2024-072-JCT.json').",
      call. = FALSE
    )
  }

  message("Downloading ", filename, " MRR file data...")

  x <- ptagis_GET(paste0("files/mrr/", filename))

  # --- normalize downloaded content ---
  as_text <- function(x) {
    if (inherits(x, "response")) {
      httr::content(x, as = "text", encoding = "UTF-8")
    } else if (is.raw(x)) {
      rawToChar(x)
    } else if (is.character(x) && length(x) >= 1L) {
      x[[1L]]
    } else {
      stop(
        "Unexpected object from ptagis_GET(). Classes: ",
        paste(class(x), collapse = ", "),
        call. = FALSE
      )
    }
  }

  ext <- tolower(tools::file_ext(filename))

  # --- parse by file type ---
  if (ext == "json") {

    # P5 JSON (most recent / canonical)
    if (is.list(x) && !inherits(x, "response")) {
      json_obj <- x
    } else {
      json_obj <- jsonlite::fromJSON(as_text(x), simplifyVector = FALSE)
    }

    out <- parse_mrr_json(json_obj)

  } else if (ext == "xml") {

    # P4 XML (legacy)
    xml_text <- as_text(x)
    doc <- xml2::read_xml(xml_text)

    out <- parse_mrr_xml(doc)

  } else {

    # P3 ASCII / TXT (oldest, least reliable)
    txt <- as_text(x)

    # defensive sniff
    if (grepl("^\\s*<", txt)) {
      stop("File appears to be XML but lacks a .xml extension.", call. = FALSE)
    }
    if (grepl("^\\s*\\{", txt)) {
      stop("File appears to be JSON but lacks a .json extension.", call. = FALSE)
    }

    out <- parse_mrr_txt(txt)
  }

  # --- enforce canonical event schema i.e., intended from JSON ---
  out <- enforce_schema(out)

  # --- optionally drop PDV-related objects ---
  if (isTRUE(drop_pdvs)) {
    out$session_pdv_fields <- NULL
    out$detail_pdv_fields  <- NULL
    out$pdv_values         <- NULL
  }

  # --- return ---
  out

}

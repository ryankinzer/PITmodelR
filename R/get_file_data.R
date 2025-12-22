#' @title Download and Parse a PTAGIS MRR File
#'
#' @description
#' Downloads a single PTAGIS mark–recapture–recovery (MRR) file and parses it
#' into structured components. Parsing is file-type specific (JSON, XML, TXT),
#' while schema enforcement is centralized at the end of this function.
#'
#' @param filename Character; name of the MRR file (e.g., "CDR-2024-072-JCT.json").
#' @param return Character; one of "list" (default), "xml", "session", "events".
#'
#' @return Parsed MRR data according to `return`.
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

  # --- optionally drop PDV-related objects
  if (isTRUE(drop_pdvs)) {
    out$session_pdv_fields <- NULL
    out$detail_pdv_fields  <- NULL
    out$pdv_values         <- NULL
  }

  # --- return ---
  out

}

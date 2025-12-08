#' @title Download and Parse a MRR File
#'
#' @description
#' Downloads a single PTAGIS mark–recapture–recovery (MRR) XML file and parses it
#' into a structured list of tibbles. The file is retrieved from the PTAGIS API
#' using its filename (e.g., `"CDR-2024-072-JCT.xml"`).
#'
#' @param filename Character; the name of the MRR file to download.
#' @param return Character; format of the returned object. One of:
#'   `"list"` (default), `"xml"`, `"session"`, or `"events"`.
#'
#' @return
#' If `return = "list"`, a list containing:
#' \itemize{
#'   \item `session` — one-row tibble of session-level metadata
#'   \item `events` — tibble of MRR event records
#'   \item `session_pdv_fields` — mapping of session-level project-defined fields
#'   \item `detail_pdv_fields` — mapping of event-level project-defined fields
#' }
#' For other `return` values, the corresponding component is returned.
#'
#' @author Ryan Kinzer
#'
#' @export

get_file_data <- function(
    filename,
    return = c("list", "xml", "session", "events")
) {

  # validate filename
  if (missing(filename) ||
      !is.character(filename) ||
      length(filename) != 1L ||
      !nzchar(filename)) {
    stop("`filename` must be a single non-empty string (e.g., 'CDR-2024-072-JCT.xml').",
         call. = FALSE)
  }

  return <- match.arg(return)

  message("Downloading ", filename, " data...")

  x <- ptagis_GET(paste0("files/mrr/", filename))

  # Decide by file extension (we'll need to re-think this for many .txt files w/out extensions)
  ext <- tolower(tools::file_ext(filename))

  if (ext == "xml") {
    # ---------- XML PATH ----------
    xml_text <- NULL
    if (inherits(x, "response")) {
      xml_text <- httr::content(x, as = "text", encoding = "UTF-8")
    } else if (is.raw(x)) {
      xml_text <- rawToChar(x)
    } else if (is.character(x) && length(x) >= 1L) {
      xml_text <- x[[1L]]
    } else {
      stop("Unexpected object from ptagis_GET(): cannot parse as XML.", call. = FALSE)
    }

    doc <- xml2::read_xml(xml_text)

    if (return == "xml") return(doc)

    out <- parse_mrr_xml(doc)

  } else if (ext == "json") {
    # ---------- JSON PATH ----------
    # ptagis_GET may already return a parsed list, but handle other cases too.
    if (is.list(x) && !inherits(x, "response")) {
      json_obj <- x
    } else if (inherits(x, "response")) {
      file_text <- httr::content(x, as = "text", encoding = "UTF-8")
      json_obj  <- jsonlite::fromJSON(file_text, simplifyVector = FALSE)
    } else if (is.raw(x)) {
      json_obj <- jsonlite::fromJSON(rawToChar(x), simplifyVector = FALSE)
    } else if (is.character(x) && length(x) >= 1L) {
      json_obj <- jsonlite::fromJSON(x[[1L]], simplifyVector = FALSE)
    } else {
      stop("Unexpected object from ptagis_GET(): cannot parse as JSON.\n",
           "Classes: ", paste(class(x), collapse = ", "),
           call. = FALSE)
    }

    if (return == "xml") {
      stop("return = 'xml' is not valid for JSON input.", call. = FALSE)
    }

    out <- parse_mrr_json(json_obj)

  } else {
    stop("Unknown file extension. Expected '.xml' or '.json'.", call. = FALSE)
  }

  switch(return,
         list    = out,
         session = out$session,
         events  = out$events
  )
}

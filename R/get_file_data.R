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

get_file_data <- function(
    filename,
    return = c("list", "xml", "session", "events")
) {

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

  return <- match.arg(return)

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

    if (return == "xml") {
      stop("return = 'xml' is not valid for JSON input.", call. = FALSE)
    }

    out <- parse_mrr_json(json_obj)

  } else if (ext == "xml") {

    # P4 XML (legacy)
    xml_text <- as_text(x)
    doc <- xml2::read_xml(xml_text)

    if (return == "xml") return(doc)

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
  # NOTE: XML/TXT compatibility will be revisited later
  out$events <- enforce_schema(out$events)

  # --- return ---
  switch(
    return,
    list    = out,
    session = out$session,
    events  = out$events
  )
}

# get_file_data <- function(
#     filename,
#     return = c("list", "xml", "session", "events")
# ) {
#
#   # validate filename
#   if (missing(filename) ||
#       !is.character(filename) ||
#       length(filename) != 1L ||
#       !nzchar(filename)) {
#     stop("`filename` must be a single non-empty string (e.g., 'CDR-2024-072-JCT.xml').",
#          call. = FALSE)
#   }
#
#   return <- match.arg(return)
#
#   message("Downloading ", filename, " data...")
#
#   x <- ptagis_GET(paste0("files/mrr/", filename))
#
#   # normalize downloaded content to character text when needed
#   as_text <- function(x) {
#     if (inherits(x, "response")) {
#       httr::content(x, as = "text", encoding = "UTF-8")
#     } else if (is.raw(x)) {
#       rawToChar(x)
#     } else if (is.character(x) && length(x) >= 1L) {
#       x[[1L]]
#     } else {
#       stop(
#         "Unexpected object from ptagis_GET(). Classes: ",
#         paste(class(x), collapse = ", "),
#         call. = FALSE
#       )
#     }
#   }
#
#   # decide by file extension (XML & JSON are reliable; TXT is not)
#   ext <- tolower(tools::file_ext(filename))
#
#   if (ext == "xml") {
#     # ---------- XML PATH ----------
#     xml_text <- as_text(x)
#     doc <- xml2::read_xml(xml_text)
#
#     if (return == "xml") return(doc)
#
#     out <- parse_mrr_xml(doc)
#     # xml_text <- NULL
#     # if (inherits(x, "response")) {
#     #   xml_text <- httr::content(x, as = "text", encoding = "UTF-8")
#     # } else if (is.raw(x)) {
#     #   xml_text <- rawToChar(x)
#     # } else if (is.character(x) && length(x) >= 1L) {
#     #   xml_text <- x[[1L]]
#     # } else {
#     #   stop("Unexpected object from ptagis_GET(): cannot parse as XML.", call. = FALSE)
#     # }
#     #
#     # doc <- xml2::read_xml(xml_text)
#     #
#     # if (return == "xml") return(doc)
#     #
#     # out <- parse_mrr_xml(doc)
#
#   } else if (ext == "json") {
#     # ---------- JSON PATH ----------
#     if (is.list(x) && !inherits(x, "response")) {
#       json_obj <- x
#     } else {
#       json_obj <- jsonlite::fromJSON(as_text(x), simplifyVector = FALSE)
#     }
#
#     if (return == "xml") {
#       stop("return = 'xml' is not valid for JSON input.", call. = FALSE)
#     }
#
#     out <- parse_mrr_json(json_obj)
#     # ptagis_GET may already return a parsed list, but handle other cases too.
#     # if (is.list(x) && !inherits(x, "response")) {
#     #   json_obj <- x
#     # } else if (inherits(x, "response")) {
#     #   file_text <- httr::content(x, as = "text", encoding = "UTF-8")
#     #   json_obj  <- jsonlite::fromJSON(file_text, simplifyVector = FALSE)
#     # } else if (is.raw(x)) {
#     #   json_obj <- jsonlite::fromJSON(rawToChar(x), simplifyVector = FALSE)
#     # } else if (is.character(x) && length(x) >= 1L) {
#     #   json_obj <- jsonlite::fromJSON(x[[1L]], simplifyVector = FALSE)
#     # } else {
#     #   stop("Unexpected object from ptagis_GET(): cannot parse as JSON.\n",
#     #        "Classes: ", paste(class(x), collapse = ", "),
#     #        call. = FALSE)
#     # }
#     #
#     # if (return == "xml") {
#     #   stop("return = 'xml' is not valid for JSON input.", call. = FALSE)
#     # }
#     #
#     # out <- parse_mrr_json(json_obj)
#
#   } else {
#     # ---------- P3 ASCII / TXT PATH ----------
#     # any non-XML / non-JSON file is treated as a P3 text file
#     txt <- as_text(x)
#
#     # defensive sniff
#     if (grepl("^\\s*<", txt)) {
#       stop(
#         "File appears to be XML but does not have .xml extension.",
#         call. = FALSE
#       )
#     }
#     if (grepl("^\\s*\\{", txt)) {
#       stop(
#         "File appears to be JSON but does not have .json extension.",
#         call. = FALSE
#       )
#     }
#
#     out <- parse_mrr_txt(txt)
#     #stop("Unknown file extension. Expected '.xml' or '.json'.", call. = FALSE)
#   }
#
#   switch(
#     return,
#     list    = out,
#     session = out$session,
#     events  = out$events
#   )
# }

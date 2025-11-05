#' @title Download and Parse a MRR File
#'
#' @description Download and parse data within a single PTAGIS mark-recapture-recovery
#' (MRR) .xml file.
#'
#' @param filename name of the file to be downloaded as character
#' e.g. "CDR-2024-072-JCT.xml".
#' @param return the format of the object to be returned:
#' "list" (default), "xml", "session", or "events"
#'
#' @return if "list", a list with tibbles: session, events, session_pdv_fields,
#' detail_pdv_fields
#'
#' @export

get_file_data <- function(filename,
                          return = c("list","xml","session","events")) {

  if (missing(filename) || !is.character(filename) || length(filename) != 1L || !nzchar(filename)) {
    stop("`filename` must be a single non-empty string, e.g. 'CDR-2024-072-JCT.xml'.", call. = FALSE)
  }
  return <- match.arg(return)

  message("Downloading ", filename, " data...")
  x <- ptagis_GET(paste0("files/mrr/", filename))

  # Ensure we have XML text to feed xml2
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

  switch(return,
         list    = out,
         session = out$session,
         events  = out$events
  )
}

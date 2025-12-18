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

  # --- validate filename ---
  if (missing(filename) ||
      !is.character(filename) ||
      length(filename) != 1L ||
      !nzchar(filename)) {
    stop("`filename` must be a single non-empty string (e.g., 'CDR-2024-072-JCT.xml').",
         call. = FALSE)
  }

  return <- match.arg(return)

  message("Downloading ", filename, " MRR file data...")

  x <- ptagis_GET(paste0("files/mrr/", filename))

  # --- normalize downloaded content to character ---
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

  # --- parse by file type (JSON, XML, or TXT) ---
  if(ext == "json") {

    # P5 JSON PATH
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

    # P4 XML PATH
    xml_text <- as_text(x)
    doc <- xml2::read_xml(xml_text)

    if (return == "xml") return(doc)

    out <- parse_mrr_xml(doc)

  } else {

    # P3 ASCII / TXT PATH
    txt <- as_text(x)

    # defensive sniff
    if (grepl("^\\s*<", txt)) {
      stop(
        "File appears to be XML but does not have .xml extension.",
        call. = FALSE
      )
    }
    if (grepl("^\\s*\\{", txt)) {
      stop(
        "File appears to be JSON but does not have .json extension.",
        call. = FALSE
      )
    }

    out <- parse_mrr_txt(txt)

  }

  # force schema function (consider moving to outside internal function later)
  enforce_event_schema <- function(events) {

    # ---- canonical event schema ----------------------------------------------
    schema <- list(
      source_system_name    = character(),
      source_system_version = character(),
      name                  = character(),
      file_name             = character(),
      created               = as.POSIXct(character(), tz = "UTC"),
      session_message       = character(),
      mrr_project           = character(),
      session_note          = character(),
      trap_start_date_time  = character(),
      trap_end_date_time    = character(),

      sequence_number       = numeric(),
      pit_tag               = character(),
      length                = numeric(),
      weight                = numeric(),
      species_run_rear_type = character(),
      rtv                   = character(),
      additional_positional = character(),
      conditional_comments  = character(),
      text_comments         = character(),
      nfish                 = character(),

      capture_method        = character(),
      event_date            = as.POSIXct(character(), tz = "UTC"),
      event_site            = character(),
      mark_method           = character(),
      mark_temperature      = numeric(),
      migration_year        = numeric(),
      organization          = character(),
      release_date          = as.POSIXct(character(), tz = "UTC"),
      release_site          = character(),
      release_temperature   = numeric(),
      tagger                = character(),
      location_rkmext       = numeric(),
      brood_year             = numeric()
    )

    # ---- empty input safeguard -----------------------------------------------
    if (is.null(events) || !is.data.frame(events) || nrow(events) == 0) {
      return(tibble::as_tibble(schema)[0, ])
    }

    events <- tibble::as_tibble(events)

    # ---- add missing columns --------------------------------------------------
    for (nm in names(schema)) {
      if (!nm %in% names(events)) {
        events[[nm]] <- schema[[nm]]
      }
    }

    # ---- coerce existing columns to schema types ------------------------------
    for (nm in intersect(names(schema), names(events))) {

      target <- schema[[nm]]

      # POSIXct
      if (inherits(target, "POSIXct")) {
        if (!inherits(events[[nm]], "POSIXct")) {
          events[[nm]] <- suppressWarnings(
            as.POSIXct(events[[nm]], tz = "UTC", origin = "1970-01-01")
          )
        }

        # numeric
      } else if (is.numeric(target)) {
        if (!is.numeric(events[[nm]])) {
          events[[nm]] <- suppressWarnings(as.numeric(events[[nm]]))
        }

        # character
      } else {
        if (!is.character(events[[nm]])) {
          events[[nm]] <- as.character(events[[nm]])
        }
      }
    }

    # ---- normalize blank comment fields --------------------------------------
    comment_cols <- c(
      "additional_positional",
      "conditional_comments",
      "text_comments"
    )

    for (cc in intersect(comment_cols, names(events))) {
      events[[cc]][!nzchar(events[[cc]])] <- NA_character_
    }

    # ---- final column order ---------------------------------------------------
    events <- events[, names(schema), drop = FALSE]

    events
  }

  # --- enforce schema guarantees ---
  out$events <- enforce_event_schema(out$events)

  # --- return ---
  switch(
    return,
    list    = out,
    session = out$session,
    events  = out$events
  )
} # end function

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

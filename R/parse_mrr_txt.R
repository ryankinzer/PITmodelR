#' @title Parse a PTAGIS MRR TXT Document
#'
#' @description
#' Converts a raw PTAGIS MRR .txt document into structured tibbles. The function
#' strips namespaces, extracts session-level metadata and event records, and performs
#' basic type coercion (datetime and numeric). This is a low-level helper used by
#' higher-level MRR import functions.
#'
#' @param .txt A .txt document representing a PTAGIS MRR .txt file.
#'
#' @return A named list containing:
#' \describe{
#'   \item{\code{session}}{Tibble of session-level metadata (one row).}
#'   \item{\code{events}}{Tibble of event-level records.}
#'   \item{\code{session_pdv_fields}}{Mapping table for session-level PDV fields.}
#'   \item{\code{detail_pdv_fields}}{Mapping table for event-level PDV fields.}
#' }
#'
#' @keywords internal

parse_mrr_txt <- function(txt) {

  # read all lines
  lines <- readLines(textConnection(txt), warn = FALSE)

  # helper to extract field after colon in P3 files
  extract_field <- function(lines, label) {
    idx <- grep(paste0("^\\s*", label, "\\s*:"), lines, ignore.case = TRUE)
    if (length(idx) == 0) return(NA_character_)

    pieces <- strsplit(lines[idx[1]], ":", fixed = TRUE)[[1]]
    if (length(pieces) < 2) return(NA_character_)

    value <- trimws(paste(pieces[-1], collapse = ":"))
    if (value == "") NA_character_ else value
  }

  # helpers for date and number coercion/parsing
  parse_dt <- function(x) {
    if (is.na(x) || !nzchar(x)) return(NA_real_)
    # first try direct parsing with 2-digit year
    dt <- suppressWarnings(lubridate::parse_date_time(
      x,
      orders = c("mdy HM", "mdy HMS"),
      tz = "America/Los_Angeles"
    ))
    # ff parsed with year < 100, convert to 2000s
    if (!is.na(dt) && lubridate::year(dt) < 100) {
      lubridate::year(dt) <- lubridate::year(dt) + 2000
    }
    dt
  }
  parse_two_digit_year <- function(x) {
    x <- trimws(x)
    # missing or non-numeric
    if (is.na(x) || !nzchar(x)) return(NA_integer_)
    # tolerate 1 or 2 digit entries; reject everything else
    if (!grepl("^[0-9]{1,2}$", x)) return(NA_integer_)
    yy <- as.integer(x)
    2000 + yy # convert 0-99 -> 2000-2099
  }
  parse_num <- function(x) {
    suppressWarnings(as.numeric(x))
  }

  #-----------------------------
  # Extract Session-Level Fields

  # program and version
  prog <- extract_field(lines, "PROGRAM VERSION")
  source_system_name    <- NA_character_
  source_system_version <- NA_character_
  if (!is.na(prog)) {
    parts <- strsplit(prog, "\\s+")[[1]]
    if (length(parts) >= 2) {
      source_system_name    <- parts[1]
      source_system_version <- parts[2]
    }
  }

  # session message
  dash_count <- stringr::str_count(lines, "-")
  dash_idx   <- which(dash_count >= 10)
  session_message <- NA_character_
  if (length(dash_idx) >= 2) {
    msg_lines <- lines[(dash_idx[1] + 1):(dash_idx[2] - 1)]
    msg_lines <- trimws(msg_lines)
    msg_lines <- msg_lines[nzchar(msg_lines)]
    if (length(msg_lines)) {
      session_message <- paste(msg_lines, collapse = " ")
    }
  }

  # session header fields (mandatory)
  file_name           <- extract_field(lines, "FILE TITLE")
  name                <- sub("\\.txt$", "", file_name)
  created             <- parse_dt(extract_field(lines, "CLOSE DATE"))
  mrr_project         <- extract_field(lines, "COORDINATOR ID")
  capture_method      <- extract_field(lines, "CAPTURE METHOD")
  event_date          <- parse_dt(extract_field(lines, "TAG DATE"))
  event_site          <- extract_field(lines, "TAG SITE")
  mark_method         <- extract_field(lines, "TAGGING METHOD")
  mark_temperature    <- parse_num(extract_field(lines, "TAGGING TEMP"))
  migration_year      <- parse_two_digit_year(extract_field(lines, "MIGRATORY YR"))
  organization        <- extract_field(lines, "ORGANIZATION")
  release_date        <- parse_dt(extract_field(lines, "RELEASE DATE"))
  release_site        <- extract_field(lines, "RELEASE SITE")
  release_temperature <- parse_num(extract_field(lines, "RELEASE WATER TEMP"))
  tagger              <- extract_field(lines, "TAGGER")
  #location_rkm        <- sub("\\.[^.]*$", "", extract_field(lines, "RELEASE RIVER KM"))
  location_rkmext     <- parse_num(sub(".*\\.(\\d{3})$", "\\1",
                                       extract_field(lines, "RELEASE RIVER KM")))

  # session header fields (optional)
  hatchery            <- extract_field(lines, "HATCHERY SITE")
  stock               <- extract_field(lines, "STOCK")
  brood_year          <- parse_two_digit_year(extract_field(lines, "BROOD YR"))
  raceway_transect    <- extract_field(lines, "RACEWAY/TRANSECT")
  post_tag_temperature<- parse_num(extract_field(lines, "POST TAGGING TEMP"))

  #------------------------------
  # Pain-In-The-Butt Session Note

  relkm_idx <- grep("^\\s*RELEASE RIVER KM", lines)

  session_note <- NA_character_

  # retrieve session note
  if(length(relkm_idx)) {

    # find first event line after RELEASE RIVER KM
    event_idx   <- grep("^\\s*[0-9]+\\s+", lines)
    first_event <- event_idx[event_idx > relkm_idx][1]

    if (!is.na(first_event) && first_event > relkm_idx + 1) {
      raw_notes <- lines[(relkm_idx + 1):(first_event - 1)]
      raw_notes <- trimws(raw_notes)
      raw_notes <- raw_notes[nzchar(raw_notes)]  # remove empty strings

      if (length(raw_notes)) {
        session_note <- paste(raw_notes, collapse = " ")
      }
    }
  } # end session note

  #------------
  # Event Block

  # identify event rows and assemble
  is_event_row <- grepl("^\\s*[0-9]{1,4}\\s", lines)
  event_lines <- lines[is_event_row]

  # event row parser (based on P3 file spec doc) https://www.ptagis.org/content/DataSpecification/topics/p3-file.htm
  parse_event_line <- function(x) {
    # extract comments first
    comments_raw <- strsplit(trimws(substr(x, 46, nchar(x))), "\\|")[[1]]
    # make sure we have 3 comments
    comments_raw <- c(comments_raw, rep(NA, 3 - length(comments_raw)))

    list(
      sequence_number       = as.integer(trimws(substr(x, 1, 4))),
      pit_tag               = trimws(substr(x, 7, 20)),
      length                = suppressWarnings(as.integer(trimws(substr(x, 21, 28)))),
      weight                = suppressWarnings(as.numeric(trimws(substr(x, 29, 38)))),
      species_run_rear_type = trimws(substr(x, 41, 43)),
      # release time variable
      rtv                   = trimws(substr(x, 44, 45)),
      additional_positional = trimws(comments_raw[1]),
      conditional_comments  = trimws(comments_raw[2]),
      text_comments         = trimws(comments_raw[3])
    )
  }

  # parse events
  events <- map_df(event_lines, parse_event_line)

  # attach inherited session fields to events
  events <- events %>%
    dplyr::mutate(
      capture_method = capture_method,
      event_date = event_date,
      event_site = event_site,
      mark_method = mark_method,
      mark_temperature = mark_temperature,
      migration_year = migration_year,
      organization = organization,
      release_date = release_date,
      release_site = release_site,
      release_temperature = release_temperature,
      tagger = tagger,
      location_rkmext = location_rkmext,
      brood_year = brood_year
      # others created above?
    )

  #---------------------
  # Build Session Tibble
  session <- tibble::tibble(
    source_system_name = source_system_name,
    source_system_version = source_system_version,
    name = name,
    file_name = file_name,
    created = created,
    session_message = session_message,
    mrr_project = mrr_project,
    session_note = session_note
  )

  # these do not exist in P3 files
  session_pdv_fields <- tibble::tibble()
  detail_pdv_fields  <- tibble::tibble()

  list(
    session            = session,
    events             = events,
    session_pdv_fields = session_pdv_fields,
    detail_pdv_fields  = detail_pdv_fields
  )

  # TO-DO:
  # 1. Deal w/ RELEASE TIME VARIABLE & VARIABLE RELEASE TIME DECLARATION RECORDS
  # 2. Deal w/ Note Records (within events), if needed
  # 4. add event_type, life_stage?

}


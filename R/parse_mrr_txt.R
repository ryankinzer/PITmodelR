#' @title Parse a PTAGIS MRR TXT Document
#'
#' @description
#' Converts a raw PTAGIS MRR TXT (P3) document into structured tibbles. The function
#' strips namespaces, extracts session-level metadata and event records, and performs
#' basic type coercion (datetime and numeric). This is a low-level helper used by
#' higher-level MRR import functions.
#'
#' @param txt Character vector containing the contents of a PTAGIS MRR .txt file from P3
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

  stopifnot(is.character(txt), length(txt) >= 1)

  # --- read all lines ---
  lines <- readLines(textConnection(txt), warn = FALSE)

  # --- helper functions ---

  # extract field after colon in P3 files
  extract_field <- function(lines, label) {
    rx  <- paste0("^\\s*", label, "\\s*:")
    idx <- grep(rx, lines, ignore.case = TRUE)
    if (!length(idx)) return(NA_character_)
    value <- trimws(sub(rx, "", lines[idx[1]], ignore.case = TRUE))
    if (!nzchar(value)) NA_character_ else value
  }

  # minimal datetime parsing (legacy P3 ambiguity requires it)
  # additional coercion done in enforce_schema()
  parse_dt <- function(x) {
    if (is.na(x) || !nzchar(x)) return(NA)
    dt <- suppressWarnings(
      lubridate::parse_date_time(
        x,
        orders = c("mdy HM", "mdy HMS"),
        tz = "America/Los_Angeles"       # Pacific Standard Time (PST): the standard for P3 files
      )
    )
    if (is.na(dt)) return(NA)
    if (lubridate::year(dt) < 100) {
      lubridate::year(dt) <- lubridate::year(dt) + 2000
    }
    dt
  }

  # resolve two-digit years, returned as character (converted to numeric in enforce_schema())
  parse_two_digit_year <- function(x) {
    x <- trimws(x)
    if (is.na(x) || !nzchar(x)) return(NA_character_)    # missing or non-numeric
    if (!grepl("^[0-9]{1,2}$", x)) return(NA_character_) # tolerate 1 or 2 digit entries; reject everything else
    sprintf("%02d", as.integer(x))
  }

  # --- extract session-level fields ---

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
  extract_session_message <- function(lines, max_lines = 10) {
    # limit search to header region
    hdr <- lines[seq_len(min(length(lines), max_lines))]
    dash_idx <- which(stringr::str_count(hdr, "-") >= 15)
    if(length(dash_idx) < 2) return(NA_character_)

    # message lines are between first two dashed lines
    msg <- trimws(hdr[(dash_idx[1] + 1):(dash_idx[2] - 1)])
    msg <- msg[nzchar(msg)]
    if (!length(msg)) return(NA_character_)
    msg <- paste(msg, collapse = " ")
    if (nchar(msg) > 76) msg <- substr(msg, 1, 76)
    msg
  }

  # extract session message
  session_message <- extract_session_message(lines)

  # mandatory P3 session header fields (re-mapped below)
  file_name           <- extract_field(lines, "FILE TITLE")
  mrr_project         <- extract_field(lines, "COORDINATOR ID")
  name                <- sub("\\.txt$", "", file_name)
  created             <- parse_dt(extract_field(lines, "CLOSE DATE"))

  capture_method      <- extract_field(lines, "CAPTURE METHOD")
  event_date          <- parse_dt(extract_field(lines, "TAG DATE"))
  event_site          <- extract_field(lines, "TAG SITE")
  mark_method         <- extract_field(lines, "TAGGING METHOD")
  mark_temperature    <- extract_field(lines, "TAGGING TEMP")
  migration_year      <- parse_two_digit_year(extract_field(lines, "MIGRATORY YR"))
  organization        <- extract_field(lines, "ORGANIZATION")
  release_date        <- parse_dt(extract_field(lines, "RELEASE DATE"))
  release_site        <- extract_field(lines, "RELEASE SITE")
  release_temperature <- extract_field(lines, "RELEASE WATER TEMP")
  tagger              <- extract_field(lines, "TAGGER")

  location_rkm        <- extract_field(lines, "RELEASE RIVER KM")
  location_rkmext     <- if (!is.na(location_rkm)) {
    sub(".*\\.(\\d{3})$", "\\1", location_rkm)
  } else NA_character_

  # optional P3 session header fields
  hatchery            <- extract_field(lines, "HATCHERY SITE")
  stock               <- extract_field(lines, "STOCK")
  brood_year          <- parse_two_digit_year(extract_field(lines, "BROOD YR"))
  raceway             <- extract_field(lines, "RACEWAY/TRANSECT")
  holding_temperature <- extract_field(lines, "POST TAGGING TEMP")

  # --- session note extraction ---
  relkm_idx <- grep("^\\s*RELEASE RIVER KM", lines)
  close_idx <- grep("^\\s*CLOSE DATE\\s*:", lines)

  session_note <- NA_character_

  # retrieve session note
  if (length(relkm_idx)) {

    event_rows  <- which(grepl("^\\s{0,3}[0-9]{1,4}\\s{2}", lines))

    note_start <- relkm_idx + 1
    note_end   <- min(
      if (length(event_rows)) event_rows[1] - 1 else Inf,
      if (length(close_idx)) close_idx[1] - 1 else Inf,
      length(lines)
    )
    if (note_end >= note_start) {
      raw <- trimws(lines[note_start:note_end])
      raw <- raw[nzchar(raw)]
      if (length(raw)) session_note <- paste(raw, collapse = " ")
    }
  }

  # --- variable release time (vrt) declarations ---
  is_vrt_row <- grepl("^\\s{4}V\\d{2}=\\d{2}/\\d{2}/\\d{2} \\d{2}:\\d{2}", lines)

  vrt_table <- if (any(is_vrt_row)) {
    purrr::map_df(lines[is_vrt_row], function(x) {
      tibble::tibble(
        rtv = substr(x, 6, 7),
        vrt_datetime = parse_dt(substr(x, 9, 22))
      )
    })
  } else {
    tibble::tibble(rtv = character(), vrt_datetime = as.POSIXct(character()))
  }

  # --- event block ---
  is_event_row <- grepl("^\\s{0,3}[0-9]{1,4}\\s{2}", lines)
  if (length(close_idx)) {
    is_event_row[seq_along(lines) >= close_idx[1]] <- FALSE
  }

  # lines of event rows
  event_lines <- lines[is_event_row]

  # event row parser (no numeric coercion; done later)
  parse_event_line <- function(x) {
    comments_raw <- strsplit(trimws(substr(x, 46, nchar(x))), "\\|")[[1]] # extract comments first
    comments_raw <- c(comments_raw, rep(NA_character_, 3 - length(comments_raw))) # make sure we have 3 comments

    list(
      sequence_number       = trimws(substr(x, 1, 4)),
      pit_tag               = trimws(substr(x, 7, 20)),
      length                = trimws(substr(x, 21, 28)),
      weight                = trimws(substr(x, 29, 38)),
      species_run_rear_type = trimws(substr(x, 41, 43)),
      rtv                   = trimws(substr(x, 44, 45)),
      additional_positional = comments_raw[1],
      conditional_comments  = comments_raw[2],
      text_comments         = comments_raw[3],
      nfish                 = "1"
    )
  }

  events <- if (length(event_lines)) {
    purrr::map_df(event_lines, parse_event_line)
  } else {
    tibble::tibble()
  }

  # attach inherited session fields to events
  if (nrow(events)) {
    events <- dplyr::mutate(
      events,
      capture_method      = capture_method,
      event_date          = event_date,
      event_site          = event_site,
      mark_method         = mark_method,
      mark_temperature    = mark_temperature,
      migration_year      = migration_year,
      organization        = organization,
      release_date        = release_date,
      release_temperature = release_temperature,
      tagger              = tagger,
      location_rkmext     = location_rkmext,
      brood_year          = brood_year
    )
  }

  # overwrite release_date if variable release times provided
  if (nrow(events) && nrow(vrt_table)) {
    events <- events |>
      dplyr::left_join(vrt_table, by = "rtv") |>
      dplyr::mutate(release_date = dplyr::coalesce(vrt_datetime, release_date)) |>
      dplyr::select(-vrt_datetime)
  }

  # --- build session tibble ---
  session <- tibble::tibble(
    source_system_name    = source_system_name,
    source_system_version = source_system_version,
    name                  = name,
    file_name             = file_name,
    created               = created,
    session_message       = session_message,
    mrr_project           = mrr_project,
    session_note          = session_note
    # trap_start_date_time  = as.character(event_date),
    # trap_end_date_time    = as.character(created)
  )

  list(
    session            = session,
    events             = events,
    # these do not exist in P3 files (empty tibbles)
    session_pdv_fields = tibble::tibble(),
    detail_pdv_fields  = tibble::tibble()
  )
}

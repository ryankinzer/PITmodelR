#' @title Enforce schema on PTAGIS MRR file parsed data
#'
#' @description
#' Applies a canonical column schema to parsed PTAGIS MRR file outputs.
#' This function is intentionally run *after* parsing (JSON, XML, or TXT)
#' to standardize column presence, order, and data types in a single place.
#'
#' Parsers are expected to be flexible and lossless; this function is the
#' authoritative schema gatekeeper.
#'
#' Currently, the enforced schema reflects all known fields available in
#' PTAGIS **MRR JSON** files. Additional fields from XML or TXT files may be
#' added later.
#'
#' @param parsed A named list containing at least:
#'   \describe{
#'     \item{session}{A tibble with session-level metadata (0–1 rows).}
#'     \item{events}{A tibble with event-level records (0+ rows).}
#'     \item{session_pdv_fields}{(optional) Session-level PDV mapping table.}
#'     \item{detail_pdv_fields}{(optional) Event-level PDV mapping table.}
#'   }
#' @param tz Time zone to enforce for all datetime columns (default `"UTC"`).
#'
#' @return The input list with `session` and `events` coerced to the
#'   enforced schema. PDV tables are returned unchanged.
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' out <- parse_mrr_json(obj)
#' out <- enforce_schema(out)
#' }

enforce_schema <- function(parsed, tz = "UTC") {

  stopifnot(
    is.list(parsed),
    "session" %in% names(parsed),
    "events" %in% names(parsed)
  )

  # --- internal helper functions ----
  coerce_chr <- function(x) {
    if (is.null(x)) return(NA_character_)
    as.character(x)
  }

  coerce_dbl <- function(x) {
    if (is.null(x)) return(NA_real_)
    suppressWarnings(as.numeric(x))
  }

  coerce_dttm <- function(x) {
    if (is.null(x)) return(as.POSIXct(NA, tz = tz))
    if (inherits(x, "POSIXct")) return(x)
    parse_datetime_mixed(x)
  }

  # --- JSON session schema (authoritative) ---
  session_schema <- list(
    schema                = "chr",
    source_system_name    = "chr",
    source_system_version = "chr",
    name                  = "chr",
    file_name             = "chr",
    legacy_file_name      = "chr",
    created               = "dttm",
    modified              = "dttm",
    session_message       = "chr",
    mrr_project           = "chr",
    session_note          = "chr"
  )

  # --- JSON event schema (authoritative) ---
  event_schema <- list(
    # typical fields observed
    sequence_number       = "dbl",
    pit_tag               = "chr",
    event_date            = "dttm",
    event_type            = "chr",
    event_site            = "chr",
    organization          = "chr",
    capture_method        = "chr",
    species_run_rear_type = "chr",
    life_stage            = "chr",
    length                = "dbl",
    location_rkmext       = "dbl",
    mark_method           = "chr",
    mark_temperature      = "dbl",
    migration_year        = "dbl",
    release_date          = "dttm",
    release_site          = "chr",
    release_temperature   = "dbl",
    tagger                = "chr",
    weight                = "dbl",
    conditional_comments  = "chr",
    text_comments         = "chr",

    # optional / extended JSON fields
    acoustic_tag          = "chr",
    brood_year            = "dbl",
    cw_tag                = "chr",
    detail_note           = "chr",
    genetic_id            = "chr",
    hatchery              = "chr",
    holding_temperature   = "dbl",
    location_source       = "chr",
    location_latitude     = "dbl",
    location_longitude    = "dbl",
    other_tag             = "chr",
    raceway               = "chr",
    radio_tag             = "chr",
    location_rkm_offset   = "dbl",
    location_rkm          = "dbl",
    scale_id              = "chr",
    second_pit_tag        = "chr",
    spawn_year            = "dbl",
    stock                 = "chr"
  )

  # --- enforce session schema ---
  session <- parsed$session
  if (!inherits(session, "data.frame")) {
    session <- tibble::tibble()
  }

  for (nm in names(session_schema)) {
    if (!nm %in% names(session)) {
      session[[nm]] <- NA
    }
    session[[nm]] <- switch(
      session_schema[[nm]],
      chr  = coerce_chr(session[[nm]]),
      dttm = coerce_dttm(session[[nm]])
    )
  }

  session <- session[, names(session_schema), drop = FALSE]

  # --- enforce event schema ---
  events <- parsed$events
  if (!inherits(events, "data.frame")) {
    events <- tibble::tibble()
  }

  for (nm in names(event_schema)) {
    if (!nm %in% names(events)) {
      events[[nm]] <- NA
    }
    events[[nm]] <- switch(
      event_schema[[nm]],
      chr  = coerce_chr(events[[nm]]),
      dbl  = coerce_dbl(events[[nm]]),
      dttm = coerce_dttm(events[[nm]])
    )
  }

  events <- events[, names(event_schema), drop = FALSE]

  # --- return standardized sessions & events ---
  parsed$session <- session
  parsed$events  <- events

  parsed
}

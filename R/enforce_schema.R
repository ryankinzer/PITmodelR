#' @description
#' Enforces a canonical column set and data types for parsed PTAGIS MRR data.
#' This function should be called exactly once per file, after parsing
#' (JSON, XML, or TXT), and before downstream use or row-binding.
#'
#' Parsers are intentionally flexible; this function is the single point
#' where schema is standardized.
#'
#' @param out A list produced by parse_mrr_json(), parse_mrr_xml(), or
#'   parse_mrr_txt(), containing:
#'   \itemize{
#'     \item session
#'     \item events
#'     \item session_pdv_fields
#'     \item detail_pdv_fields
#'   }
#'
#' @return A list with the same structure as \code{out}, but with enforced
#'   schemas for \code{session} and \code{events}.
#'
#' @keywords internal

enforce_schema <- function(out) {

  # --- validation ---
  stopifnot(
    is.list(out),
    all(c("session", "events",
          "session_pdv_fields", "detail_pdv_fields") %in% names(out))
  )

  # --- canonical SESSION schema (JSON-defined) ---
  session_schema <- list(
    schema                = character(),
    source_system_name    = character(),
    source_system_version = character(),
    name                  = character(),
    file_name             = character(),
    legacy_file_name      = character(),
    created               = as.POSIXct(character(), tz = "UTC"),
    modified              = as.POSIXct(character(), tz = "UTC"),
    session_message       = character(),
    mrr_project           = character(),
    session_note          = character()
  )

  # --- canonical EVENTS schema ---
  events_schema <- list(
    sequence_number       = numeric(),
    pit_tag               = character(),
    event_date            = as.POSIXct(character(), tz = "UTC"),
    event_type            = character(),
    event_site            = character(),
    organization          = character(),
    capture_method        = character(),
    species_run_rear_type = character(),
    life_stage            = character(),
    length                = numeric(),
    location_rkmext       = numeric(),
    mark_method           = character(),
    mark_temperature      = numeric(),
    migration_year        = numeric(),
    release_date          = as.POSIXct(character(), tz = "UTC"),
    release_site          = character(),
    release_temperature   = numeric(),
    tagger                = character(),
    weight                = numeric(),
    conditional_comments  = character(),
    text_comments         = character(),

    # optional / extended JSON fields
    acoustic_tag          = character(),
    brood_year            = numeric(),
    cw_tag                = character(),
    detail_note           = character(),
    genetic_id            = character(),
    hatchery              = character(),
    holding_temperature   = numeric(),
    location_source       = character(),
    location_latitude     = numeric(),
    location_longitude    = numeric(),
    other_tag             = character(),
    raceway               = character(),
    radio_tag             = character(),
    location_rkm_offset   = numeric(),
    location_rkm          = numeric(),
    scale_id              = character(),
    second_pit_tag        = character(),
    spawn_year            = numeric(),
    stock                 = character()
  )

  # --- internal helper: enforce a schema on a tibble ---
  enforce_tbl <- function(df, schema) {

    # empty or missing table
    if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
      return(tibble::as_tibble(schema)[0, ])
    }

    df <- tibble::as_tibble(df)

    # add missing columns safely
    for (nm in names(schema)) {
      if (!nm %in% names(df)) {
        target <- schema[[nm]]
        n <- nrow(df)

        if (inherits(target, "POSIXct")) {
          df[[nm]] <- as.POSIXct(rep(NA, n), tz = attr(target, "tzone"))
        } else if (is.numeric(target)) {
          df[[nm]] <- rep(NA_real_, n)
        } else if (is.character(target)) {
          df[[nm]] <- rep(NA_character_, n)
        } else if (is.logical(target)) {
          df[[nm]] <- rep(NA, n)
        } else {
          df[[nm]] <- rep(NA, n)
        }
      }
    }

    # coerce types
    for (nm in names(schema)) {
      target <- schema[[nm]]
      x <- df[[nm]]

      if (inherits(target, "POSIXct")) {
        if (!inherits(x, "POSIXct")) {
          df[[nm]] <- suppressWarnings(
            as.POSIXct(x, tz = "UTC", origin = "1970-01-01")
          )
        }
      } else if (is.numeric(target)) {
        if (!is.numeric(x)) df[[nm]] <- suppressWarnings(as.numeric(x))
      } else {
        if (!is.character(x)) df[[nm]] <- as.character(x)
      }
    }

    # normalize blank comment fields
    comment_cols <- intersect(c("conditional_comments", "text_comments"), names(df))
    for (cc in comment_cols) {
      df[[cc]][!nzchar(df[[cc]])] <- NA_character_
    }

    # enforce column order
    df[, names(schema), drop = FALSE]
  }

  # apply schemas
  out$session <- enforce_tbl(out$session, session_schema)
  out$events  <- enforce_tbl(out$events,  events_schema)

  out
}

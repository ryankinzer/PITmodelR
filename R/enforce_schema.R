#' Enforce Schema on Parsed JSON, XML, TXT Files
#'
#' @description
#' Enforces a canonical column set and data types for parsed PTAGIS MRR data.
#' Parsers are intentionally flexible; this function is the single point
#' where schema is standardized and PDVs are safely captured.#'
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
#' @return A list with enforced schema and preserved SPDV and PDV values (if present).
#'
#' @keywords internal

enforce_schema <- function(out) {

  # --- validation ---
  stopifnot(
    is.list(out),
    all(c("session",
          "events",
          "session_pdv_fields",
          "detail_pdv_fields"
    ) %in% names(out))
  )

  # --- canonical SESSION schema (JSON-defined) ---
  session_schema <- list(
    schema                = character(),
    source_system_name    = character(),
    source_system_version = character(),
    file_name             = character(),
    legacy_file_name      = character(),
    file_version          = character(), # P4 XML
    mrr_project           = character(),
    name                  = character(),
    session_message       = character(),
    session_note          = character(),
    created               = as.POSIXct(character(), tz = "UTC"),
    modified              = as.POSIXct(character(), tz = "UTC")
  )

  # --- canonical EVENTS schema ---
  events_schema <- list(
    sequence_number       = integer(),
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
    location_rkm          = character(),
    scale_id              = character(),
    second_pit_tag        = character(),
    spawn_year            = numeric(),
    stock                 = character()
  )

  # --- normalize SPDV and PDV metadata column names
  if (nrow(out$session_pdv_fields)) {
    out$session_pdv_fields$pdv_column <-
      tolower(out$session_pdv_fields$pdv_column)
  }

  if (nrow(out$detail_pdv_fields)) {
    out$detail_pdv_fields$pdv_column <-
      tolower(out$detail_pdv_fields$pdv_column)
  }

  # --- extract PDV values BEFORE schema enforcement
  pdv_values <- list(
    session = tibble::tibble(),
    events  = tibble::tibble()
  )

  # --- session PDVs ---
  if (is.data.frame(out$session) && nrow(out$session) == 1) {

    spdv_cols <- grep("^spdv\\d+$", names(out$session), value = TRUE)

    if (length(spdv_cols)) {
      pdv_values$session <- tibble::tibble(
        pdv_column = spdv_cols,
        value      = vapply(
          out$session[spdv_cols],
          function(x) as.character(x[[1]]),
          character(1)
        )
      )

      # remove SPDVs from session before schema enforcement
      out$session <- out$session[, setdiff(names(out$session), spdv_cols), drop = FALSE]
    }
  }

  # --- event PDVs ---
  if (is.data.frame(out$events) && nrow(out$events) > 0) {

    pdv_cols <- grep("^pdv\\d+$", names(out$events), value = TRUE)

    if (length(pdv_cols)) {

      seq_col <- if ("sequence_number" %in% names(out$events)) {
        out$events$sequence_number
      } else {
        seq_len(nrow(out$events))
      }

      pdv_values$events <- purrr::map_dfr(pdv_cols, function(nm) {
        tibble::tibble(
          sequence_number = seq_col,
          pdv_column      = nm,
          value           = as.character(out$events[[nm]])
        )
      })

      # remove PDVs from events before schema enforcement
      out$events <- out$events[, setdiff(names(out$events), pdv_cols), drop = FALSE]
    }
  }

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
  out$session    <- enforce_tbl(out$session, session_schema)
  out$events     <- enforce_tbl(out$events,  events_schema)
  out$pdv_values <- pdv_values

  out
}

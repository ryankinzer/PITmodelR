#' Enforce Schema on Parsed JSON, XML, TXT Files
#'
#' @description
#' Enforces a canonical column set and data types for parsed PTAGIS MRR data.
#' Parsers are intentionally flexible; this function is the single point
#' where schema is standardized and project-defined variables (PDVs) are safely captured.
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

  # helper to clean SPDV & PDV labels to snake_case
  clean_pdv_label <- function(x) {
    x <- as.character(x)
    x[!nzchar(x)] <- NA_character_

    x <- gsub("&", " and ", x)
    x <- gsub("@", " at ", x)
    x <- gsub("[/]+", " ", x)                     # "a / b" -> "a b"
    x <- gsub("[()\\[\\]\\{\\}]", " ", x, perl = TRUE)
    x <- gsub("[,:;]+", " ", x)
    x <- gsub("\\.+", " ", x)
    x <- gsub("([a-z0-9])([A-Z])", "\\1_\\2", x)  # camelCase -> snake_case
    x <- gsub("[^A-Za-z0-9]+", "_", x)
    x <- gsub("_+", "_", x)
    x <- gsub("^_+|_+$", "", x)
    x <- tolower(x)
    ifelse(!is.na(x) & grepl("^[0-9]", x), paste0("x_", x), x)
  }

  # --- clean SPDV and PDV column names ---
  normalize_pdv_map <- function(df) {
    if (!is.data.frame(df) || nrow(df) == 0) return(tibble::as_tibble(df))
    df <- tibble::as_tibble(df)

    if ("pdv_column" %in% names(df)) {
      # standardize to canonical lowercase: spdv1/pdv1/...
      df$pdv_column <- tolower(trimws(as.character(df$pdv_column)))
    }

    if ("label" %in% names(df)) {
      df$label_raw <- as.character(df$label)
      df$label     <- clean_pdv_label(df$label_raw)
    } else {
      df$label_raw <- NA_character_
      df$label     <- NA_character_
    }

    if (!"definition" %in% names(df)) df$definition <- NA_character_
    df
  }

  out$session_pdv_fields <- normalize_pdv_map(out$session_pdv_fields)
  out$detail_pdv_fields  <- normalize_pdv_map(out$detail_pdv_fields)

  # --- extract PDV values BEFORE schema enforcement ---
  pdv_values <- list(
    session = tibble::tibble(),
    events  = tibble::tibble()
  )

  # --- session PDVs ---
  if (is.data.frame(out$session) && nrow(out$session) == 1) {

    # if session has > 1 row (shouldn't), take first row defensively
    if (nrow(out$session) > 1) out$session <- out$session[1, , drop = FALSE]

    spdv_cols <- grep("^spdv\\d+$", names(out$session), value = TRUE, ignore.case = TRUE)
    if (length(spdv_cols)) {

      names(out$session) <- tolower(names(out$session))

      pdv_values$session <- tibble::tibble(
        spdv_column = spdv_cols,
        value       = vapply(
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

    pdv_cols <- grep("^pdv\\d+$", names(out$events), value = TRUE, ignore.case = TRUE)
    if (length(pdv_cols)) {

      seq_col <- if ("sequence_number" %in% names(out$events)) out$events$sequence_number else seq_len(nrow(out$events))

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

  # --- internal helpers: enforce a schema on a tibble ---
  na_of_type <- function(proto, n) {
    if (inherits(proto, "POSIXct")) return(as.POSIXct(rep(NA_real_, n), origin = "1970-01-01", tz = attr(proto, "tzone")))
    if (is.integer(proto))          return(rep(NA_integer_, n))
    if (is.numeric(proto))          return(rep(NA_real_, n))
    if (is.character(proto))        return(rep(NA_character_, n))
    if (is.logical(proto))          return(rep(NA, n))
    rep(NA, n)
  }

  coerce_to_proto <- function(x, proto) {
    if (inherits(proto, "POSIXct")) {
      if (inherits(x, "POSIXct")) return(x)
      return(suppressWarnings(as.POSIXct(x, tz = "UTC", origin = "1970-01-01")))
    }
    if (is.integer(proto)) return(suppressWarnings(as.integer(x)))
    if (is.numeric(proto)) return(suppressWarnings(as.numeric(x)))
    if (is.character(proto)) return(as.character(x))
    x
  }

  enforce_tbl <- function(df, schema) {
    if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
      return(tibble::as_tibble(schema)[0, ])
    }

    df <- tibble::as_tibble(df)
    n  <- nrow(df)

    # add missing
    for (nm in names(schema)) {
      if (!nm %in% names(df)) df[[nm]] <- na_of_type(schema[[nm]], n)
    }

    # coerce
    for (nm in names(schema)) {
      df[[nm]] <- coerce_to_proto(df[[nm]], schema[[nm]])
    }

    # normalize blank comment fields
    for (cc in intersect(c("conditional_comments", "text_comments"), names(df))) {
      df[[cc]] <- dplyr::na_if(trimws(as.character(df[[cc]])), "")
    }

    df[, names(schema), drop = FALSE]
  }

  out$session    <- enforce_tbl(out$session, session_schema)
  out$events     <- enforce_tbl(out$events,  events_schema)
  out$pdv_values <- pdv_values

  out
}

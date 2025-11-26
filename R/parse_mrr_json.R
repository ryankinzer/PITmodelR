#' @title Parse a PTAGIS MRR JSON Document
#'
#' @description
#' Converts a raw PTAGIS MRR JSON document into structured tibbles. The function
#' strips json namespaces, extracts session-level metadata, detail-level
#' project-defined field (PDV) mappings, and event records, and performs basic
#' type coercion (datetime and numeric). This is a low-level helper used by
#' higher-level MRR import functions.
#'
#' @param obj An \code{jsonlite::fromJSON} representing a PTAGIS MRR JSON file.
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

parse_mrr_json <- function(obj) {
  if (is.null(obj) || !is.list(obj)) {
    stop("`obj` must be a parsed JSON list from PTAGIS MRR.", call. = FALSE)
  }

  # ---- helpers (keep behavior aligned with parse_mrr_xml) ----
  to_snake <- function(x) {
    x <- gsub("\\.", "_", x)
    x <- gsub("([a-z0-9])([A-Z])", "\\1_\\2", x)
    x <- tolower(x)

    # Align JSON spdV1, spdV2, ... with XML SPDV1 → spdv1, spdv2, ...
    x <- gsub("^spd_v(\\d+)$", "spdv\\1", x)

    x
  }

  text_or_na <- function(x) {
    if (is.null(x) || length(x) == 0) NA_character_ else as.character(x)
  }

  parse_datetime_mixed <- function(x) {
    if (is.null(x)) return(as.POSIXct(character(), tz = "UTC"))
    x <- as.character(x)
    x[!nzchar(x)] <- NA_character_

    # Normalize ISO offsets "-06:00" → "-0600" for %z
    x_iso <- sub("([+-]\\d{2}):?(\\d{2})$", "\\1\\2", x, perl = TRUE)

    fmts <- c(
      "%Y-%m-%dT%H:%M:%OS%z",  # ISO with frac sec + tz
      "%Y-%m-%dT%H:%M:%S%z",   # ISO with tz
      "%Y-%m-%dT%H:%M:%OS",    # ISO frac sec, no tz
      "%Y-%m-%dT%H:%M:%S",     # ISO no tz
      "%Y-%m-%d %H:%M:%OS",    # space sep
      "%Y-%m-%d %H:%M:%S",
      "%m/%d/%Y %H:%M:%S",     # US-style
      "%m/%d/%Y %H:%M"         # US-style
    )

    out <- as.POSIXct(NA_real_, origin = "1970-01-01", tz = "UTC")
    out <- rep(out, length(x_iso))

    todo <- is.na(out) & !is.na(x_iso)
    for (f in fmts) {
      if (!any(todo)) break
      tmp <- suppressWarnings(as.POSIXct(x_iso[todo], format = f, tz = "UTC"))
      hit <- !is.na(tmp)
      if (any(hit)) out[which(todo)[hit]] <- tmp[hit]
      todo <- is.na(out) & !is.na(x_iso)
    }

    out
  }

  numify <- function(v) {
    v_chr <- as.character(v)
    z <- suppressWarnings(as.numeric(v_chr))
    ifelse(is.na(z) & !is.na(v_chr) & nzchar(v_chr), NA_real_, z)
  }

  # ---- session fields (top-level properties) ----
  drop_names <- c("mrrEvents", "sessionProjectDefinedFields", "detailProjectDefinedFields")

  session_names <- setdiff(names(obj), drop_names)
  session_vals  <- obj[session_names]
  names(session_vals) <- to_snake(names(session_vals))

  session <- tibble::as_tibble(session_vals)

  # Coerce common types like in XML parser
  for (nm_dt in c("created", "modified")) {
    if (nm_dt %in% names(session)) {
      session[[nm_dt]] <- parse_datetime_mixed(session[[nm_dt]])
    }
  }
  # If you later want numeric SPDV coercion, you can uncomment/adapt:
  # for (nm_num in c("spdv1","spdv2","spdv3","spdv4","spdv5","spdv7","spdv8","spdv9")) {
  #   if (nm_num %in% names(session)) session[[nm_num]] <- numify(session[[nm_num]])
  # }

  # ---- session PDV field map ----
  sp_list <- obj$sessionProjectDefinedFields
  session_pdv_fields <- if (!is.null(sp_list) && length(sp_list)) {
    lst <- lapply(sp_list, function(nd) {
      tibble::tibble(
        label      = text_or_na(nd$label),
        pdv_column = text_or_na(nd$pdvColumn),
        definition = text_or_na(nd$definition)
      )
    })
    tibble::as_tibble(do.call(
      rbind,
      lapply(lst, as.data.frame, stringsAsFactors = FALSE)
    ))
  } else {
    tibble::tibble(label = character(), pdv_column = character(), definition = character())
  }

  # ---- detail PDV field map ----
  dp_list <- obj$detailProjectDefinedFields
  detail_pdv_fields <- if (!is.null(dp_list) && length(dp_list)) {
    lst <- lapply(dp_list, function(nd) {
      tibble::tibble(
        label      = text_or_na(nd$label),
        pdv_column = text_or_na(nd$pdvColumn),
        definition = text_or_na(nd$definition)
      )
    })
    tibble::as_tibble(do.call(
      rbind,
      lapply(lst, as.data.frame, stringsAsFactors = FALSE)
    ))
  } else {
    tibble::tibble(label = character(), pdv_column = character(), definition = character())
  }

  # ---- events (each element of mrrEvents) ----
  ev_list <- obj$mrrEvents

  events <- if (!is.null(ev_list) && length(ev_list)) {
    rows <- lapply(ev_list, function(ev) {
      ev <- as.list(ev)
      names(ev) <- to_snake(names(ev))
      ev
    })

    # Union of all keys, then fill missing with NA
    cols <- Reduce(union, lapply(rows, names))
    rows <- lapply(rows, function(r) {
      r[setdiff(cols, names(r))] <- NA
      r[cols]
    })

    df <- tibble::as_tibble(do.call(
      rbind,
      lapply(rows, as.data.frame, stringsAsFactors = FALSE)
    ))

    # Coerce common fields (mirror XML behavior)
    for (nm_dt in c("event_date", "release_date")) {
      if (nm_dt %in% names(df)) df[[nm_dt]] <- parse_datetime_mixed(df[[nm_dt]])
    }
    for (nm_num in c(
      "length", "weight", "mark_temperature", "release_temperature",
      "brood_year", "migration_year", "sequence_number",
      "location_latitude", "location_longitude", "location_rkmext"
    )) {
      if (nm_num %in% names(df)) df[[nm_num]] <- numify(df[[nm_num]])
    }

    df
  } else {
    tibble::tibble()
  }

  list(
    session             = session,
    events              = events,
    session_pdv_fields  = session_pdv_fields,
    detail_pdv_fields   = detail_pdv_fields
  )
}

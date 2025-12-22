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

  # ---- helpers ----
  to_snake <- function(x) {
    x <- gsub("\\.", "_", x)
    x <- gsub("([a-z0-9])([A-Z])", "\\1_\\2", x)
    x <- tolower(x)
    x <- gsub("^spd_v(\\d+)$", "spdv\\1", x)
    x
  }

  text_or_na <- function(x) {
    if (is.null(x) || length(x) == 0) NA_character_ else as.character(x)
  }

  # ---- session ----
  drop_names <- c(
    "mrrEvents",
    "sessionProjectDefinedFields",
    "detailProjectDefinedFields"
  )

  session_names <- setdiff(names(obj), drop_names)
  session_vals  <- obj[session_names]
  names(session_vals) <- to_snake(names(session_vals))

  session <- tibble::as_tibble(
    lapply(session_vals, text_or_na)
  )

  # coerce session datetime fields
  for (nm_dt in c("created", "modified")) {
    if (nm_dt %in% names(session)) {
      session[[nm_dt]] <- parse_datetime_mixed(session[[nm_dt]])
    }
  }

  # ---- session PDV fields ----
  sp_list <- obj$sessionProjectDefinedFields
  session_pdv_fields <- if (!is.null(sp_list) && length(sp_list)) {
    tibble::as_tibble(do.call(
      rbind,
      lapply(sp_list, function(nd) {
        data.frame(
          label      = text_or_na(nd$label),
          pdv_column = text_or_na(nd$pdvColumn),
          definition = text_or_na(nd$definition),
          stringsAsFactors = FALSE
        )
      })
    ))
  } else {
    tibble::tibble(
      label = character(),
      pdv_column = character(),
      definition = character()
    )
  }

  # ---- detail PDV fields ----
  dp_list <- obj$detailProjectDefinedFields
  detail_pdv_fields <- if (!is.null(dp_list) && length(dp_list)) {
    tibble::as_tibble(do.call(
      rbind,
      lapply(dp_list, function(nd) {
        data.frame(
          label      = text_or_na(nd$label),
          pdv_column = text_or_na(nd$pdvColumn),
          definition = text_or_na(nd$definition),
          stringsAsFactors = FALSE
        )
      })
    ))
  } else {
    tibble::tibble(
      label = character(),
      pdv_column = character(),
      definition = character()
    )
  }

  # ---- events ----
  ev_list <- obj$mrrEvents

  events <- if (!is.null(ev_list) && length(ev_list)) {
    rows <- lapply(ev_list, function(ev) {
      ev <- as.list(ev)
      names(ev) <- to_snake(names(ev))
      lapply(ev, text_or_na)
    })

    cols <- Reduce(union, lapply(rows, names))
    rows <- lapply(rows, function(r) {
      r[setdiff(cols, names(r))] <- NA_character_
      r[cols]
    })

    tibble::as_tibble(do.call(
      rbind,
      lapply(rows, as.data.frame, stringsAsFactors = FALSE)
    ))
  } else {
    tibble::tibble()
  }

  # coerce event datetime fields
  for (nm_dt in c("event_date", "release_date")) {
    if (nm_dt %in% names(events)) {
      events[[nm_dt]] <- parse_datetime_mixed(events[[nm_dt]])
    }
  }

  list(
    session             = session,
    events              = events,
    session_pdv_fields  = session_pdv_fields,
    detail_pdv_fields   = detail_pdv_fields
  )
}

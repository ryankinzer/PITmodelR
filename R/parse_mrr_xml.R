#' @title Parse a PTAGIS MRR XML Document
#'
#' @description
#' Converts a raw PTAGIS MRR XML (P4) document into structured tibbles. The function
#' strips XML namespaces, extracts session-level metadata, session- and detail-level
#' project-defined field mappings, and event records, and performs basic
#' type coercion (datetime and numeric). This is a low-level parser used by
#' higher-level MRR import functions.
#'
#' @param doc An \code{xml2::xml_document} representing a PTAGIS MRR XML file.
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

parse_mrr_xml <- function(doc) {

  # strip namespaces to make XPath simple and robust
  xml2::xml_ns_strip(doc)

  # --- internal helper functions ---
  to_snake <- function(x) {
    x <- gsub("\\.", "_", x)
    x <- gsub("([a-z0-9])([A-Z])", "\\1_\\2", x)
    x <- tolower(x)

    # resolve known P4 XML inconsistencies; intentionally local
    fix <- c(
      "pittag"     = "pit_tag",
      "mrrproject" = "mrr_project",
      "cwtag"      = "cw_tag"
    )
    x <- ifelse(x %in% names(fix), fix[x], x)

    # prevent name collisions
    make.unique(x, sep = "_")
  }

  text_or_na <- function(node) {
    if (length(node)) xml2::xml_text(node) else NA_character_
  }

  # --- session ---
  ses_nodes <- xml2::xml_find_all(doc, ".//Session")

  if (length(ses_nodes) == 0) {
    warning("No <Session> node found in XML. Returning empty tibbles.", call. = FALSE)
    empty <- tibble::tibble()
    return(list(
      session = empty,
      events = empty,
      session_pdv_fields = empty,
      detail_pdv_fields = empty
    ))
  }

  if (length(ses_nodes) > 1) {
    warning("Multiple <Session> nodes found; using the first.", call. = FALSE)
  }

  ses <- ses_nodes[[1]]

  # simple session fields (skip complex children)
  kids <- xml2::xml_children(ses)
  nm   <- xml2::xml_name(kids)

  keep <- !nm %in% c(
    "MRREvents",
    "SessionProjectDefinedFields",
    "DetailProjectDefinedFields"
  )

  kids <- kids[keep]
  nm   <- nm[keep]

  session_vals <- vapply(kids, xml2::xml_text, character(1))
  names(session_vals) <- to_snake(nm)

  session <- tibble::as_tibble(as.list(session_vals))

  # light coercion only (schema enforcement happens later)

  # coerce common types
  for (nm_dt in c("created","modified")) {
    if (nm_dt %in% names(session)) {
      session[[nm_dt]] <- parse_datetime_mixed(session[[nm_dt]])
    }
  }

  # --- session PDV field map ---
  sp_nodes <- xml2::xml_find_all(
    ses,
    ".//SessionProjectDefinedFields/SessionProjectDefinedField"
  )

  session_pdv_fields <- if (length(sp_nodes)) {
    lst <- lapply(sp_nodes, function(nd) {
      tibble::tibble(
        label      = text_or_na(xml2::xml_find_first(nd, ".//Label")),
        pdv_column = text_or_na(xml2::xml_find_first(nd, ".//PDVColumn")),
        definition = text_or_na(xml2::xml_find_first(nd, ".//Definition"))
      )
    })
    tibble::as_tibble(do.call(rbind, lapply(lst, as.data.frame)))
  } else {
    tibble::tibble(label = character(),
                   pdv_column = character(),
                   definition = character())
  }

  # --- detail PDV field map ---
  dp_nodes <- xml2::xml_find_all(
    ses,
    ".//DetailProjectDefinedFields/DetailProjectDefinedField"
  )

  detail_pdv_fields <- if (length(dp_nodes)) {
    lst <- lapply(dp_nodes, function(nd) {
      tibble::tibble(
        label      = text_or_na(xml2::xml_find_first(nd, ".//Label")),
        pdv_column = text_or_na(xml2::xml_find_first(nd, ".//PDVColumn")),
        definition = text_or_na(xml2::xml_find_first(nd, ".//Definition"))
      )
    })
    tibble::as_tibble(do.call(rbind, lapply(lst, as.data.frame)))
  } else {
    tibble::tibble(label = character(),
                   pdv_column = character(),
                   definition = character())
  }

  # --- events (each <MRREvent>) ---
  ev_nodes <- xml2::xml_find_all(ses, ".//MRREvent")

  events <- if (length(ev_nodes)) {

    # assumes MRREvent children are flat text nodes
    rows <- lapply(ev_nodes, function(ev) {
      kids <- xml2::xml_children(ev)
      keys <- to_snake(xml2::xml_name(kids))
      vals <- vapply(kids, xml2::xml_text, character(1))
      as.list(stats::setNames(vals, keys))
    })

    # row-bind, fill missing columns
    cols <- Reduce(union, lapply(rows, names))
    rows <- lapply(rows, function(r) {
      r[setdiff(cols, names(r))] <- NA_character_
      r[cols]
    })

    df <- tibble::as_tibble(
      do.call(rbind, lapply(rows, as.data.frame, stringsAsFactors = FALSE))
    )

    # light coercion of common fields
    for (nm_dt in c("event_date","release_date")) {
      if (nm_dt %in% names(df)) {
        df[[nm_dt]] <- parse_datetime_mixed(df[[nm_dt]])
      }
    }

    df
  } else {
    tibble::tibble()
  }

  # --- return ---
  list(
    session            = session,
    events             = events,
    session_pdv_fields = session_pdv_fields,
    detail_pdv_fields  = detail_pdv_fields
  )
}

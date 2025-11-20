#' Download and parse a PTAGIS MRR XML/JSON file
#'
#' @param filename e.g. "CDR-2024-072-JCT.xml" or ".json"
#' @param return "list" (default), "xml", "session", or "events"
#' @return if "list", a list with tibbles: session, events, session_pdv_fields, detail_pdv_fields
#' @export
get_file_data <- function(filename, return = c("list","xml","session","events")) {
  if (missing(filename) || !is.character(filename) || length(filename) != 1L || !nzchar(filename)) {
    stop("`filename` must be a single non-empty string, e.g. 'CDR-2024-072-JCT.xml'.", call. = FALSE)
  }
  return <- match.arg(return)

  message("Downloading ", filename, " data...")
  x <- ptagis_GET(paste0("files/mrr/", filename))

  # Decide by file extension
  ext <- tolower(tools::file_ext(filename))

  if (ext == "xml") {
    # ---------- XML PATH ----------
    xml_text <- NULL
    if (inherits(x, "response")) {
      xml_text <- httr::content(x, as = "text", encoding = "UTF-8")
    } else if (is.raw(x)) {
      xml_text <- rawToChar(x)
    } else if (is.character(x) && length(x) >= 1L) {
      xml_text <- x[[1L]]
    } else {
      stop("Unexpected object from ptagis_GET(): cannot parse as XML.", call. = FALSE)
    }

    doc <- xml2::read_xml(xml_text)

    if (return == "xml") return(doc)

    out <- parse_mrr_xml(doc)

  } else if (ext == "json") {
    # ---------- JSON PATH ----------
    # ptagis_GET may already return a parsed list, but handle other cases too.
    if (is.list(x) && !inherits(x, "response")) {
      json_obj <- x
    } else if (inherits(x, "response")) {
      file_text <- httr::content(x, as = "text", encoding = "UTF-8")
      json_obj  <- jsonlite::fromJSON(file_text, simplifyVector = FALSE)
    } else if (is.raw(x)) {
      json_obj <- jsonlite::fromJSON(rawToChar(x), simplifyVector = FALSE)
    } else if (is.character(x) && length(x) >= 1L) {
      json_obj <- jsonlite::fromJSON(x[[1L]], simplifyVector = FALSE)
    } else {
      stop("Unexpected object from ptagis_GET(): cannot parse as JSON.\n",
           "Classes: ", paste(class(x), collapse = ", "),
           call. = FALSE)
    }

    if (return == "xml") {
      stop("return = 'xml' is not valid for JSON input.", call. = FALSE)
    }

    out <- parse_mrr_json(json_obj)

  } else {
    stop("Unknown file extension. Expected '.xml' or '.json'.", call. = FALSE)
  }

  switch(return,
         list    = out,
         session = out$session,
         events  = out$events
  )
}

#' Parse a PTAGIS MRR JSON object into tidy tibbles
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
      "brood_year", "migration_year", "sequence_number"
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

#' Parse a PTAGIS MRR xml_document into tidy tibbles
#' @keywords internal
parse_mrr_xml <- function(doc) {
  # Strip namespaces to make XPath simple and robust
  xml2::xml_ns_strip(doc)

  to_snake <- function(x) {
    x <- gsub("\\.", "_", x)
    x <- gsub("([a-z0-9])([A-Z])", "\\1_\\2", x)
    tolower(x)
  }
  text_or_na <- function(node) if (length(node)) xml2::xml_text(node) else NA_character_

  # Robust datetime parser → always returns POSIXct (UTC), NA when unparseable
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
      "%m/%d/%Y %H:%M:%S",     # US-style (e.g., "03/11/2024 12:00:00")
      "%m/%d/%Y %H:%M"         # US-style (e.g., "03/11/2024 12:00")
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

    out  # POSIXct vector (UTC), NA where parsing failed
  }

  numify <- function(v) {
    z <- suppressWarnings(as.numeric(v))
    ifelse(is.na(z) & !is.na(v) & nzchar(v), NA_real_, z)
  }

  # session (grab the first <Session> anywhere)
  ses <- xml2::xml_find_first(doc, ".//Session")
  if (length(ses) == 0) {
    # Nothing matched: return empty structures with a warning
    warning("No <Session> node found in XML. Returning empty tibbles.", call. = FALSE)
    empty <- tibble::tibble()
    return(list(session = empty, events = empty,
                session_pdv_fields = empty, detail_pdv_fields = empty))
  }

  # simple session fields (skip complex children)
  kids <- xml2::xml_children(ses)
  nm <- xml2::xml_name(kids)
  keep <- !nm %in% c("MRREvents", "SessionProjectDefinedFields", "DetailProjectDefinedFields")
  kids <- kids[keep]; nm <- nm[keep]

  session_vals <- vapply(kids, xml2::xml_text, character(1))
  names(session_vals) <- to_snake(nm)
  session <- tibble::as_tibble(as.list(session_vals))

  # Coerce common types
  for (nm_dt in c("created","modified")) {
    if (nm_dt %in% names(session)) session[[nm_dt]] <- parse_datetime_mixed(session[[nm_dt]])
  }
  # for (nm_num in c("spdv1","spdv2","spdv3","spdv4","spdv5","spdv8"))
  #   if (nm_num %in% names(session)) session[[nm_num]] <- numify(session[[nm_num]])

  # session PDV field map
  sp_nodes <- xml2::xml_find_all(ses, ".//SessionProjectDefinedFields/SessionProjectDefinedField")
  session_pdv_fields <- if (length(sp_nodes)) {
    lst <- lapply(sp_nodes, function(nd) {
      tibble::tibble(
        label      = text_or_na(xml2::xml_find_first(nd, ".//Label")),
        pdv_column = text_or_na(xml2::xml_find_first(nd, ".//PDVColumn")),
        definition = text_or_na(xml2::xml_find_first(nd, ".//Definition"))
      )
    })
    tibble::as_tibble(do.call(rbind, lapply(lst, as.data.frame)))
  } else tibble::tibble(label = character(), pdv_column = character(), definition = character())

  # detail PDV field map
  dp_nodes <- xml2::xml_find_all(ses, ".//DetailProjectDefinedFields/DetailProjectDefinedField")
  detail_pdv_fields <- if (length(dp_nodes)) {
    lst <- lapply(dp_nodes, function(nd) {
      tibble::tibble(
        label      = text_or_na(xml2::xml_find_first(nd, ".//Label")),
        pdv_column = text_or_na(xml2::xml_find_first(nd, ".//PDVColumn")),
        definition = text_or_na(xml2::xml_find_first(nd, ".//Definition"))
      )
    })
    tibble::as_tibble(do.call(rbind, lapply(lst, as.data.frame)))
  } else tibble::tibble(label = character(), pdv_column = character(), definition = character())

  # events (each <MRREvent>)
  ev_nodes <- xml2::xml_find_all(ses, ".//MRREvent")
  events <- if (length(ev_nodes)) {
    rows <- lapply(ev_nodes, function(ev) {
      kids <- xml2::xml_children(ev)
      keys <- to_snake(xml2::xml_name(kids))
      vals <- vapply(kids, xml2::xml_text, character(1))
      as.list(stats::setNames(vals, keys))
    })
    # Row-bind, fill missing columns
    cols <- Reduce(union, lapply(rows, names))
    rows <- lapply(rows, function(r) { r[setdiff(cols, names(r))] <- NA_character_; r[cols] })
    df <- tibble::as_tibble(do.call(rbind, lapply(rows, as.data.frame, stringsAsFactors = FALSE)))

    # Coerce common fields
    for (nm_dt in c("event_date","release_date")) {
      if (nm_dt %in% names(df)) df[[nm_dt]] <- parse_datetime_mixed(df[[nm_dt]])
    }
    for (nm_num in c("length","weight","mark_temperature","release_temperature",
                     "brood_year","migration_year","sequence_number")) {
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

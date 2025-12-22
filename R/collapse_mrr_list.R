#' @title Collapse per-file MRR list into combined sessions/events
#'
#' @description
#' Collapses a list of per-file parsed MRR outputs (from download_mrr_files())
#' into combined `sessions` (one row per file) and `events` (all rows).
#'
#' If `pdvs = "attach"` and PDV structures are present, attaches SPDV values
#' to `sessions` (wide: spdv1, spdv2, ...) and PDV values to `events`
#' (wide: pdv1, pdv2, ...), and returns a `mapping` tibble.
#'
#' @param mrr_list Named list of parsed file objects. Each element contains at
#'   least `session` and `events`. If PDVs were kept, elements may also contain
#'   `session_pdv_fields`, `detail_pdv_fields`, and `pdv_values`.
#' @param pdvs Character; one of `"drop"` (default) or `"attach"`.
#'
#' @return A list with `sessions` and `events`. If `pdvs="attach"` and PDVs are
#'   available, also returns `mapping`.
#'
#' @keywords internal

collapse_mrr_list <- function(mrr_list,
                              pdvs = c("drop", "attach")) {

  `%||%` <- function(x, y) if (is.null(x)) y else x
  pdvs <- match.arg(pdvs)

  if (!is.list(mrr_list) || !length(mrr_list)) {
    return(list(sessions = tibble::tibble(), events = tibble::tibble()))
  }

  nms <- names(mrr_list)
  if (is.null(nms) || any(!nzchar(nms))) nms <- as.character(seq_along(mrr_list))

  # detect whether PDV payloads exist at all
  has_pdvs <- any(vapply(
    mrr_list,
    function(mrr) {
      is.list(mrr) &&
        (is.data.frame(mrr$session_pdv_fields) || is.data.frame(mrr$detail_pdv_fields) || is.list(mrr$pdv_values))
    },
    logical(1)
  ))

  if (pdvs == "attach" && !has_pdvs) {
    # silently degrade to "drop" behavior
    pdvs <- "drop"
  }

  # --- base combine: sessions + events ---
  sessions_list <- vector("list", length(mrr_list))
  events_list   <- vector("list", length(mrr_list))

  for (i in seq_along(mrr_list)) {
    mrr <- mrr_list[[i]]
    nm  <- nms[[i]]

    s <- tibble::as_tibble(mrr$session %||% tibble::tibble())
    if (!"file_name" %in% names(s)) s$file_name <- nm
    if (nrow(s) > 1) s <- s[1, , drop = FALSE]  # one session row per file

    e <- tibble::as_tibble(mrr$events %||% tibble::tibble())
    if (!"file_name" %in% names(e)) e$file_name <- s$file_name[[1]]

    # normalize for consistent joins later
    if ("sequence_number" %in% names(e)) {
      e$sequence_number <- suppressWarnings(as.integer(e$sequence_number))
    }

    sessions_list[[i]] <- s
    events_list[[i]]   <- e
  }

  sessions <- dplyr::bind_rows(sessions_list)
  events   <- dplyr::bind_rows(events_list)

  # if not attaching PDVs, return early
  if (pdvs != "attach") {
    return(list(sessions = sessions, events = events))
  }

  # --- build mapping (ONLY when attaching PDVs) ---
  mapping_rows <- list()

  for (i in seq_along(mrr_list)) {
    mrr <- mrr_list[[i]]
    nm  <- nms[[i]]
    fn  <- if (is.data.frame(mrr$session) && "file_name" %in% names(mrr$session))
      as.character(mrr$session$file_name[[1]]) else nm

    sp <- mrr$session_pdv_fields
    if (is.data.frame(sp) && nrow(sp)) {
      mapping_rows[[length(mapping_rows) + 1L]] <- tibble::tibble(
        file_name  = fn,
        level      = "session",
        pdv_column = tolower(trimws(as.character(sp[["pdv_column"]]))),
        label_raw  = as.character(sp[["label_raw"]]),
        label      = as.character(sp[["label"]]),
        definition = if ("definition" %in% names(sp)) as.character(sp[["definition"]]) else NA_character_
      )
    }

    dp <- mrr$detail_pdv_fields
    if (is.data.frame(dp) && nrow(dp)) {
      mapping_rows[[length(mapping_rows) + 1L]] <- tibble::tibble(
        file_name  = fn,
        level      = "event",
        pdv_column = tolower(trimws(as.character(dp[["pdv_column"]]))),
        label_raw  = as.character(dp[["label_raw"]]),
        label      = as.character(dp[["label"]]),
        definition = if ("definition" %in% names(dp)) as.character(dp[["definition"]]) else NA_character_
      )
    }
  }

  mapping <- if (length(mapping_rows)) dplyr::bind_rows(mapping_rows) else {
    tibble::tibble(
      file_name  = character(),
      level      = character(),
      pdv_column = character(),
      label      = character(),
      definition = character()
    )
  }

  # --- attach SPDVs to sessions (wide: spdv1, spdv2, ...) ---
  spdv_long <- list()
  for (i in seq_along(mrr_list)) {
    mrr <- mrr_list[[i]]
    nm  <- nms[[i]]
    fn  <- if (is.data.frame(mrr$session) && "file_name" %in% names(mrr$session))
      as.character(mrr$session$file_name[[1]]) else nm

    x <- mrr$pdv_values$session
    if (is.data.frame(x) && nrow(x)) {
      x <- tibble::as_tibble(x)
      if (all(c("spdv_column", "value") %in% names(x))) {
        spdv_long[[length(spdv_long) + 1L]] <- tibble::tibble(
          file_name   = fn,
          spdv_column = tolower(trimws(as.character(x[["spdv_column"]]))),
          value       = dplyr::na_if(as.character(x[["value"]]), "")
        )
      }
    }
  }
  spdv_long <- if (length(spdv_long)) dplyr::bind_rows(spdv_long) else tibble::tibble()

  if (nrow(spdv_long)) {
    spdv_wide <- tidyr::pivot_wider(
      spdv_long,
      names_from  = "spdv_column",
      values_from = "value"
    )
    sessions <- dplyr::left_join(sessions, spdv_wide, by = "file_name")
  }

  # --- attach PDVs to events (wide: pdv1, pdv2, ...) ---
  pdv_long <- list()
  for (i in seq_along(mrr_list)) {
    mrr <- mrr_list[[i]]
    nm  <- nms[[i]]
    fn  <- if (is.data.frame(mrr$session) && "file_name" %in% names(mrr$session))
      as.character(mrr$session$file_name[[1]]) else nm

    x <- mrr$pdv_values$events
    if (is.data.frame(x) && nrow(x)) {
      x <- tibble::as_tibble(x)
      if (all(c("sequence_number", "pdv_column", "value") %in% names(x))) {
        pdv_long[[length(pdv_long) + 1L]] <- tibble::tibble(
          file_name        = fn,
          sequence_number  = suppressWarnings(as.integer(x[["sequence_number"]])),
          pdv_column       = tolower(trimws(as.character(x[["pdv_column"]]))),
          value            = dplyr::na_if(as.character(x[["value"]]), "")
        )
      }
    }
  }
  pdv_long <- if (length(pdv_long)) dplyr::bind_rows(pdv_long) else tibble::tibble()

  if (nrow(pdv_long) && all(c("file_name", "sequence_number") %in% names(events))) {
    pdv_wide <- tidyr::pivot_wider(
      pdv_long,
      names_from  = "pdv_column",
      values_from = "value"
    )
    events <- dplyr::left_join(events, pdv_wide, by = c("file_name", "sequence_number"))
  }

  list(sessions = sessions,
       events = events,
       mapping = mapping)
}

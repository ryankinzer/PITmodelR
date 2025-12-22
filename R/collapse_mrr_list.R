#' @title Collapse per-file MRR list into combined sessions/events, optionally attaching PDVs
#'
#' @description
#' Collapses a list of per-file parsed MRR outputs (from download_mrr_files())
#' into combined `sessions` (one row per file) and `events` (all rows).
#' Optionally attaches SPDV values to sessions (wide by spdv1..)
#' and PDV values to events (wide by pdv1..), and returns a simple mapping tibble.
#'
#' @param mrr_list Named list of parsed file objects. Each element contains `session` and `events`,
#'   and if drop_pdvs=FALSE also `session_pdv_fields`, `detail_pdv_fields`, `pdv_values`.
#' @param attach_pdvs Logical; attach PDVs/SPDVs if present.
#' @param keep_mapping Logical; return mapping tibble if present.
#'
#' @return A list with `sessions`, `events`, and (optionally) `mapping`.
#'
#' @keywords internal
collapse_mrr_list <- function(mrr_list, attach_pdvs = FALSE, keep_mapping = TRUE) {

  `%||%` <- function(x, y) if (is.null(x)) y else x

  if (!is.list(mrr_list) || !length(mrr_list)) {
    out <- list(sessions = tibble::tibble(), events = tibble::tibble())
    if (isTRUE(keep_mapping)) {
      out$mapping <- tibble::tibble(
        file_name = character(),
        level = character(),
        pdv_column = character(),
        label = character(),
        definition = character()
      )
    }
    return(out)
  }

  nms <- names(mrr_list)
  if (is.null(nms) || any(!nzchar(nms))) nms <- as.character(seq_along(mrr_list))

  # --- base combine: sessions + events ---
  sessions_list <- vector("list", length(mrr_list))
  events_list   <- vector("list", length(mrr_list))

  for (i in seq_along(mrr_list)) {
    mrr <- mrr_list[[i]]
    nm  <- nms[[i]]

    s <- tibble::as_tibble(mrr$session %||% tibble::tibble())
    # ensure file_name exists for joining later
    if (!"file_name" %in% names(s)) s$file_name <- nm
    # sessions should be 1 row per file; keep first row defensively
    if (nrow(s) > 1) s <- s[1, , drop = FALSE]

    e <- tibble::as_tibble(mrr$events %||% tibble::tibble())
    if (!"file_name" %in% names(e)) e$file_name <- s$file_name[[1]]

    # normalize sequence_number type for consistent joining
    if ("sequence_number" %in% names(e)) {
      e$sequence_number <- suppressWarnings(as.integer(e$sequence_number))
    }

    sessions_list[[i]] <- s
    events_list[[i]]   <- e
  }

  sessions <- dplyr::bind_rows(sessions_list)
  events   <- dplyr::bind_rows(events_list)

  # --- mapping ---
  mapping <- tibble::tibble(
    file_name  = character(),
    level      = character(),
    pdv_column = character(),
    label      = character(),
    definition = character()
  )

  if (isTRUE(attach_pdvs)) {

    # build mapping from session_pdv_fields + detail_pdv_fields
    map_rows <- vector("list", length(mrr_list) * 2)
    k <- 0L

    for (i in seq_along(mrr_list)) {
      mrr <- mrr_list[[i]]
      nm  <- nms[[i]]
      fn  <- if ("file_name" %in% names(mrr$session)) as.character(mrr$session$file_name[[1]]) else nm

      sp <- mrr$session_pdv_fields
      if (is.data.frame(sp) && nrow(sp)) {
        k <- k + 1L
        map_rows[[k]] <- tibble::tibble(
          file_name = fn,
          level = "session",
          pdv_column = tolower(trimws(as.character(sp$pdv_column))),
          label = as.character(sp$label),
          definition = if ("definition" %in% names(sp)) as.character(sp$definition) else NA_character_
        )
      }

      dp <- mrr$detail_pdv_fields
      if (is.data.frame(dp) && nrow(dp)) {
        k <- k + 1L
        map_rows[[k]] <- tibble::tibble(
          file_name = fn,
          level = "event",
          pdv_column = tolower(trimws(as.character(dp$pdv_column))),
          label = as.character(dp$label),
          definition = if ("definition" %in% names(dp)) as.character(dp$definition) else NA_character_
        )
      }
    }

    map_rows <- map_rows[seq_len(k)]
    if (length(map_rows)) {
      mapping <- dplyr::bind_rows(map_rows)
    }

    # --- attach SPDVs to sessions (wide: spdv1, spdv2, ...) ---
    spdv_long <- vector("list", length(mrr_list))
    for (i in seq_along(mrr_list)) {
      mrr <- mrr_list[[i]]
      nm  <- nms[[i]]
      fn  <- if ("file_name" %in% names(mrr$session)) as.character(mrr$session$file_name[[1]]) else nm

      x <- mrr$pdv_values$session
      if (is.data.frame(x) && nrow(x)) {
        x <- tibble::as_tibble(x)
        # expected cols: spdv_column, value
        spdv_long[[i]] <- dplyr::transmute(
          x,
          file_name = fn,
          spdv_column = tolower(trimws(as.character(.data$spdv_column))),
          value = dplyr::na_if(as.character(.data$value), "")
        )
      } else {
        spdv_long[[i]] <- NULL
      }
    }
    spdv_long <- dplyr::bind_rows(spdv_long)

    if (is.data.frame(spdv_long) && nrow(spdv_long)) {
      spdv_wide <- tidyr::pivot_wider(
        spdv_long,
        names_from  = "spdv_column",
        values_from = "value"
      )
      sessions <- dplyr::left_join(sessions, spdv_wide, by = "file_name")
    }

    # --- attach PDVs to events (wide: pdv1, pdv2, ...) ---
    pdv_long <- vector("list", length(mrr_list))
    for (i in seq_along(mrr_list)) {
      mrr <- mrr_list[[i]]
      nm  <- nms[[i]]
      fn  <- if ("file_name" %in% names(mrr$session)) as.character(mrr$session$file_name[[1]]) else nm

      x <- mrr$pdv_values$events
      if (is.data.frame(x) && nrow(x)) {
        x <- tibble::as_tibble(x)
        # expected cols: sequence_number, pdv_column, value
        pdv_long[[i]] <- dplyr::transmute(
          x,
          file_name = fn,
          sequence_number = suppressWarnings(as.integer(.data$sequence_number)),
          pdv_column = tolower(trimws(as.character(.data$pdv_column))),
          value = dplyr::na_if(as.character(.data$value), "")
        )
      } else {
        pdv_long[[i]] <- NULL
      }
    }
    pdv_long <- dplyr::bind_rows(pdv_long)

    if (is.data.frame(pdv_long) && nrow(pdv_long) &&
        all(c("file_name","sequence_number") %in% names(events))) {

      pdv_wide <- tidyr::pivot_wider(
        pdv_long,
        names_from  = "pdv_column",
        values_from = "value"
      )

      events <- dplyr::left_join(
        events,
        pdv_wide,
        by = c("file_name", "sequence_number")
      )
    }
  }

  out <- list(sessions = sessions, events = events)
  if (isTRUE(keep_mapping)) out$mapping <- mapping
  out
}

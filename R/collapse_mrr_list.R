#' @title Collapse Per-File MRR Results into Combined Sessions and Events
#'
#' @description
#' Collapse a named list of per-file parsed PTAGIS mark–recapture–recovery (MRR)
#' results (typically from \code{\link{download_mrr_files}}) into two combined tibbles:
#' \code{sessions} (one row per file) and \code{events} (all event rows).
#'
#' If \code{pdvs = "attach"} and PDV values were retained upstream (i.e.,
#' the per-file objects include \code{pdv_values}), SPDV values are attached to
#' \code{sessions} in wide format (\code{spdv1}, \code{spdv2}, ...), and PDV values
#' are attached to \code{events} in wide format (\code{pdv1}, \code{pdv2}, ...).
#' In that case, a per-file \code{pdv_map} is also returned, mapping PDV code columns
#' to cleaned labels and definitions.
#'
#' @param mrr_list Named list of per-file MRR objects (output of \code{\link{get_file_data}}),
#'   typically from \code{\link{download_mrr_files}}. Names should be filenames.
#'   Each element must contain \code{session} and \code{events}. If PDVs were retained
#'   upstream, elements also contain \code{session_pdv_fields}, \code{detail_pdv_fields},
#'   and \code{pdv_values}.
#' @param pdvs Character; one of \code{"drop"} (default) or \code{"attach"}.
#'   If \code{"drop"}, return only \code{sessions} and \code{events}.
#'   If \code{"attach"}, attach \code{spdv*}/\code{pdv*} code columns when available,
#'   and return \code{pdv_map}.
#'
#' @return A list with:
#' \itemize{
#'   \item \code{sessions}: tibble with one row per file/session (optionally with \code{spdv*} columns)
#'   \item \code{events}: tibble of all event rows (optionally with \code{pdv*} columns)
#'   \item \code{pdv_map}: tibble mapping (only when \code{pdvs="attach"})
#' }
#' The \code{pdv_map} contains \code{file_name}, \code{level} (session/event),
#' \code{pdv_column}, \code{label_raw}, \code{label} (cleaned), and \code{definition}.
#'
#' @author Mike Ackerman
#'
#' @export
collapse_mrr_list <- function(mrr_list,
                              pdvs = c("drop", "attach")) {

  `%||%` <- function(x, y) if (is.null(x)) y else x
  pdvs <- match.arg(pdvs)

  # --- previous structures enforced ---
  stopifnot(is.list(mrr_list), length(mrr_list) > 0)
  nms <- names(mrr_list)
  stopifnot(!is.null(nms), all(nzchar(nms)))

  # --- helper: standard file_name per element ---
  file_name_for <- function(mrr, fallback) {
    s <- mrr$session
    if (is.data.frame(s) && nrow(s) > 0 && "file_name" %in% names(s)) {
      return(as.character(s$file_name[[1]]))
    }
    fallback
  }

  # --- detect whether PDV objects exist ---
  has_pdvs <- any(vapply(
    mrr_list,
    function(mrr) is.list(mrr) && is.list(mrr$pdv_values),
    logical(1)
  ))

  # silently degrade to "drop" behavior
  if (pdvs == "attach" && !has_pdvs) pdvs <- "drop"

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

  # --- build mapping (pdv_map) ONLY when attaching PDVs ---
  map_rows <- vector("list", length(mrr_list) * 2L)
  k <- 0L

  for (i in seq_along(mrr_list)) {
    mrr <- mrr_list[[i]]
    fn  <- file_name_for(mrr, nms[[i]])

    sp <- mrr$session_pdv_fields
    if (is.data.frame(sp) && nrow(sp)) {
      k <- k + 1L
      map_rows[[k]] <- tibble::tibble(
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
      k <- k + 1L
      map_rows[[k]] <- tibble::tibble(
        file_name  = fn,
        level      = "event",
        pdv_column = tolower(trimws(as.character(dp[["pdv_column"]]))),
        label_raw  = as.character(dp[["label_raw"]]),
        label      = as.character(dp[["label"]]),
        definition = if ("definition" %in% names(dp)) as.character(dp[["definition"]]) else NA_character_
      )
    }
  }

  pdv_map <- if (k > 0L) dplyr::bind_rows(map_rows[seq_len(k)]) else {
    tibble::tibble(
      file_name  = character(),
      level      = character(),
      pdv_column = character(),
      label_raw  = character(),
      label      = character(),
      definition = character()
    )
  }

  # --- attach SPDVs to sessions ---
  spdv_rows <- vector("list", length(mrr_list))

  for (i in seq_along(mrr_list)) {
    mrr <- mrr_list[[i]]
    fn  <- file_name_for(mrr, nms[[i]])

    x <- mrr$pdv_values$session
    if (is.data.frame(x) && nrow(x) && all(c("spdv_column", "value") %in% names(x))) {
      x <- tibble::as_tibble(x)
      spdv_rows[[i]] <- tibble::tibble(
        file_name   = fn,
        spdv_column = tolower(trimws(as.character(x[["spdv_column"]]))),
        value       = dplyr::na_if(as.character(x[["value"]]), "")
      )
    } else {
      spdv_rows[[i]] <- NULL
    }
  }

  spdv_long <- dplyr::bind_rows(spdv_rows)
  if (nrow(spdv_long)) {
    spdv_wide <- tidyr::pivot_wider(
      spdv_long,
      names_from  = "spdv_column",
      values_from = "value"
    )
    sessions <- dplyr::left_join(sessions, spdv_wide, by = "file_name")
  }

  # --- attach PDVs to events ---
  pdv_rows <- vector("list", length(mrr_list))

  for (i in seq_along(mrr_list)) {
    mrr <- mrr_list[[i]]
    fn  <- file_name_for(mrr, nms[[i]])

    x <- mrr$pdv_values$events
    if (is.data.frame(x) && nrow(x) && all(c("sequence_number", "pdv_column", "value") %in% names(x))) {
      x <- tibble::as_tibble(x)
      pdv_rows[[i]] <- tibble::tibble(
        file_name       = fn,
        sequence_number = suppressWarnings(as.integer(x[["sequence_number"]])),
        pdv_column      = tolower(trimws(as.character(x[["pdv_column"]]))),
        value           = dplyr::na_if(as.character(x[["value"]]), "")
      )
    } else {
      pdv_rows[[i]] <- NULL
    }
  }

  pdv_long <- dplyr::bind_rows(pdv_rows)
  if (nrow(pdv_long) && all(c("file_name", "sequence_number") %in% names(events))) {
    pdv_wide <- tidyr::pivot_wider(
      pdv_long,
      names_from  = "pdv_column",
      values_from = "value"
    )
    events <- dplyr::left_join(events, pdv_wide, by = c("file_name", "sequence_number"))
  }

  list(
    sessions = sessions,
    events = events,
    pdv_map = pdv_map
  )
}

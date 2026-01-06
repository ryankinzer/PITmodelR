#' @title Map PDV/SPDV code columns to mapped label columns
#'
#' @description
#' Given output from `collapse_mrr_list(pdvs = "attach")` (sessions/events contain
#' spdv*/pdv* columns and `pdv_map` contains per-file mappings), create new
#' label-based columns (using `pdv_map$label`) and drop the original code columns.
#'
#' Mapping is explicit and per-file:
#'   (file_name, level, pdv_column) -> label
#'
#' @param out List with `sessions`, `events`, and `pdv_map`.
#'
#' @return `out` with updated `sessions`/`events`. `pdv_map` is preserved.
#'
#' @keywords internal
map_pdvs_to_cols <- function(out) {

  # if pdv_map isn't present, do nothing
  if (is.null(out$pdv_map) || !is.data.frame(out$pdv_map) || nrow(out$pdv_map) == 0) {
    return(out)
  }

  sessions <- tibble::as_tibble(out$sessions)
  events   <- tibble::as_tibble(out$events)
  pdv_map  <- tibble::as_tibble(out$pdv_map)

  rx_spdv <- "^(?i)spdv\\d+$"
  rx_pdv  <- "^(?i)pdv\\d+$"

  # build minimal mapping used for joins (keep out$pdv_map unchanged)
  map_key <- pdv_map |>
    dplyr::transmute(
      file_name  = as.character(file_name),
      level      = as.character(level),
      pdv_column = tolower(trimws(as.character(pdv_column))),
      label      = as.character(label)
    ) |>
    # some defensive cleaning
    dplyr::filter(!is.na(label) & nzchar(label)) |>
    dplyr::distinct(file_name, level, pdv_column, .keep_all = TRUE)

  map_session <- dplyr::filter(map_key, level == "session") |>
    dplyr::select(file_name, pdv_column, label)

  map_event <- dplyr::filter(map_key, level == "event") |>
    dplyr::select(file_name, pdv_column, label)

  # --- sessions: spdv* -> label ---
  if ("file_name" %in% names(sessions) && any(grepl(rx_spdv, names(sessions), perl = TRUE))) {

    spdv_long <- sessions |>
      tidyr::pivot_longer(
        cols = tidyselect::matches(rx_spdv),
        names_to = "pdv_column",
        values_to = "value",
        values_drop_na = FALSE
      ) |>
      dplyr::mutate(
        pdv_column = tolower(trimws(as.character(pdv_column))),
        value      = dplyr::na_if(as.character(value), "")
      ) |>
      dplyr::left_join(map_session, by = c("file_name", "pdv_column")) |>
      dplyr::filter(!is.na(label) & nzchar(label)) |>
      dplyr::select(file_name, label, value)

    spdv_wide <- tidyr::pivot_wider(
      spdv_long,
      names_from  = label,
      values_from = value,
      values_fn   = dplyr::first
    )

    # drop spdv codes BEFORE join → no .x/.y suffixes possible
    sessions <- sessions |>
      dplyr::select(-tidyselect::matches(rx_spdv)) |>
      dplyr::left_join(spdv_wide, by = "file_name")
  }

  # --- events: pdv* -> label ---
  if (all(c("file_name", "sequence_number") %in% names(events)) &&
      any(grepl(rx_pdv, names(events), perl = TRUE))) {

    pdv_long <- events |>
      tidyr::pivot_longer(
        cols = tidyselect::matches(rx_pdv),
        names_to = "pdv_column",
        values_to = "value",
        values_drop_na = FALSE
      ) |>
      dplyr::mutate(
        pdv_column      = tolower(trimws(as.character(pdv_column))),
        sequence_number = suppressWarnings(as.integer(sequence_number)),
        value           = dplyr::na_if(as.character(value), "")
      ) |>
      dplyr::left_join(map_event, by = c("file_name", "pdv_column")) |>
      dplyr::filter(!is.na(label) & nzchar(label)) |>
      dplyr::select(file_name, sequence_number, label, value)

    pdv_wide <- tidyr::pivot_wider(
      pdv_long,
      names_from  = label,
      values_from = value,
      values_fn   = dplyr::first
    )

    # drop pdv codes BEFORE join → no .x/.y suffixes possible
    events <- events |>
      dplyr::select(-tidyselect::matches(rx_pdv)) |>
      dplyr::left_join(pdv_wide, by = c("file_name", "sequence_number"))
  }

  out$sessions <- sessions
  out$events   <- events
  out
}

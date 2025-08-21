#' Build MARK capture histories from tag events using a flexible survival map
#'
#' @param tag_history  data.frame/tibble with at least tag_code and site_code; event_time optional for ordering
#' @param locs_def     either a character vector of occasion labels in downstream order
#'                     OR a named list: names = occasion labels, each element = vector of site codes in that occasion
#' @param site_col     column name with site codes (default "site_code")
#' @param tag_col      column name with tag codes  (default "tag_code")
#' @param time_col     optional time column to order within tag (POSIXct or parseable)
#' @param enforce_order logical; if TRUE keep first occurrence of each *increasing* occasion per tag
#' @param keep_unknown  logical; if FALSE drop events whose site isn't in the survival map
#' @return list(ch_data = tibble(tag_code, ch),
#'              ch_freq = tibble(ch, freq),
#'              mapping = tibble(site_code, occasion, occ_idx),
#'              dropped_summary = tibble(site_code, n))
build_mark_histories <- function(tag_history,
                                 locs_def,
                                 site_col = "site_code",
                                 tag_col  = "tag_code",
                                 time_col = NULL,
                                 enforce_order = TRUE,
                                 keep_unknown  = FALSE) {
  stopifnot(is.data.frame(tag_history))
  stopifnot(all(c(site_col, tag_col) %in% names(tag_history)))

  # ---- 1) Build mapping: site_code -> (occasion, occ_idx) ----
  if (is.list(locs_def)) {
    # Named list: each name is an occasion label; values are site codes
    if (is.null(names(locs_def)) || any(!nzchar(names(locs_def)))) {
      stop("locs_def named list must have non-empty names (occasion labels).", call. = FALSE)
    }
    occasion <- names(locs_def)
    occ_idx  <- seq_along(locs_def)
    map <- lapply(seq_along(locs_def), function(i) {
      data.frame(site_code = as.character(locs_def[[i]]),
                 occasion  = occasion[i],
                 occ_idx   = occ_idx[i],
                 stringsAsFactors = FALSE)
    })
    mapping <- do.call(rbind, map)
  } else if (is.character(locs_def)) {
    # Simple vector: each element is an occasion label AND its unique site code
    mapping <- data.frame(
      site_code = as.character(locs_def),
      occasion  = as.character(locs_def),
      occ_idx   = seq_along(locs_def),
      stringsAsFactors = FALSE
    )
  } else {
    stop("locs_def must be a character vector OR a named list of character vectors.", call. = FALSE)
  }

  # Normalization
  mapping$site_code <- toupper(trimws(mapping$site_code))
  mapping$occasion  <- as.character(mapping$occasion)

  # ---- 2) Join mapping; drop/flag unknown sites ----
  df <- tag_history
  df[[site_col]] <- toupper(trimws(as.character(df[[site_col]])))
  df[[tag_col]]  <- as.character(df[[tag_col]])

  df2 <- dplyr::left_join(
    df,
    mapping,
    by = setNames("site_code", site_col)
  )

  dropped <- df2[is.na(df2$occ_idx), , drop = FALSE]
  if (!keep_unknown && nrow(dropped) > 0) {
    df2 <- df2[!is.na(df2$occ_idx), , drop = FALSE]
  }

  # ---- 3) Optional: enforce downstream monotone order per tag ----
  # Keep only the first appearance of each strictly increasing occ_idx per tag.
  if (enforce_order) {
    # order within tag (by time if available, else by occ_idx)
    if (!is.null(time_col) && time_col %in% names(df2)) {
      if (!inherits(df2[[time_col]], "POSIXt")) {
        suppressWarnings(df2[[time_col]] <- as.POSIXct(df2[[time_col]], tz = "UTC"))
      }
      df2 <- df2[order(df2[[tag_col]], df2[[time_col]], df2$occ_idx), , drop = FALSE]
    } else {
      df2 <- df2[order(df2[[tag_col]], df2$occ_idx), , drop = FALSE]
    }

    df2 <- df2 |>
      dplyr::group_by(.data[[tag_col]]) |>
      dplyr::arrange(.data[[tag_col]], .by_group = TRUE) |>
      # use integer default (0L); occasions start at 1 so first one always passes
      dplyr::mutate(prev_max = cummax(dplyr::lag(as.integer(occ_idx), default = 0L))) |>
      dplyr::filter(occ_idx > prev_max) |>
      dplyr::select(-prev_max) |>
      dplyr::ungroup()
  }

  # Reduce to (tag, occ_idx) and deduplicate
  surv_dat <- df2 |>
    dplyr::distinct(.data[[tag_col]], occ_idx, .keep_all = FALSE) |>
    dplyr::select(all_of(tag_col), occ_idx)

  # ---- 4) Build encounter histories ----
  n_occasions <- length(unique(mapping$occ_idx))
  occasion_cols <- as.character(seq_len(n_occasions))

  # Wide 0/1 per occasion
  ch_data <- surv_dat |>
    dplyr::mutate(detected = 1L, occ_idx = as.character(occ_idx)) |>
    tidyr::pivot_wider(
      id_cols     = dplyr::all_of(tag_col),
      names_from  = occ_idx,
      values_from = detected,
      values_fill = 0L
    )

  # Ensure all columns exist and are ordered
  miss <- setdiff(occasion_cols, names(ch_data))
  if (length(miss)) ch_data[miss] <- 0L
  ch_data <- ch_data |>
    dplyr::select(dplyr::all_of(tag_col), dplyr::all_of(occasion_cols)) |>
    dplyr::arrange(.data[[tag_col]]) |>
    dplyr::mutate(
      ch = do.call(paste0, lapply(dplyr::across(dplyr::all_of(occasion_cols)), as.character))
    ) |>
    dplyr::select(dplyr::all_of(tag_col), ch)

  # Collapsed frequency table
  ch_freq <- ch_data |>
    dplyr::count(ch, name = "freq") |>
    dplyr::arrange(dplyr::desc(freq))

  # Summary of dropped/unknown sites
  dropped_summary <- if (nrow(dropped)) {
    as.data.frame(table(dropped[[site_col]]), stringsAsFactors = FALSE) |>
      stats::setNames(c("site_code", "n")) |>
      dplyr::as_tibble()
  } else {
    tibble::tibble(site_code = character(), n = integer())
  }

  list(
    ch_data         = ch_data,
    ch_freq         = ch_freq,
    mapping         = tibble::as_tibble(mapping),
    dropped_summary = dropped_summary
  )
}

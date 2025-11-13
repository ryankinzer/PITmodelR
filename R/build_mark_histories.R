#' @title Build MARK Capture Histories from Tag Events
#'
#' @description
#' Converts tag-level observation events into capture histories compatible with
#' program MARK. Can handle multiple sites per occasion, enforce downstream order,
#' and optionally drop unknown sites.
#'
#' @param tag_history A data.frame or tibble of tag observations. Must contain
#'   columns for tag codes and site codes. A time column is optional for ordering events.
#' @param locs_def Either a character vector of occasion labels in downstream order,
#'   or a named list where names are occasion labels and each element is a vector of site codes
#'   included in that occasion.
#' @param site_col Character; column name in \code{tag_history} containing site codes.
#'   Default is \code{"site_code"}.
#' @param tag_col Character; column name in \code{tag_history} containing PIT tag codes.
#'   Default is \code{"tag_code"}.
#' @param time_col Character; optional column name in \code{tag_history} containing
#'   event timestamps (POSIXct or parseable) used to order events within tag.
#' @param enforce_order Logical; if TRUE, only the first occurrence of each
#'   strictly increasing occasion per tag is kept. Default is TRUE.
#' @param keep_unknown Logical; if FALSE, drop events with sites not in the survival map.
#'   Default is FALSE.
#'
#' @return A list with four elements:
#' \itemize{
#'   \item \code{ch_data} – tibble with one row per tag and a \code{ch} column for
#'     encounter history, plus 0/1 columns per occasion.
#'   \item \code{ch_freq} – tibble of encounter history strings and their frequencies.
#'   \item \code{mapping} – tibble mapping \code{site_code} to \code{occasion} and numeric
#'     \code{occ_idx}.
#'   \item \code{dropped_summary} – tibble summarizing the number of events dropped because
#'     their site was not in the "study design".
#' }
#'
#' @author Ryan Kinzer
#'
#' @export

build_mark_histories <- function(tag_history,
                                 locs_def,
                                 site_col = "site_code",
                                 tag_col  = "tag_code",
                                 time_col = NULL,
                                 enforce_order = TRUE,
                                 keep_unknown  = FALSE) {

  stopifnot(is.data.frame(tag_history))
  stopifnot(all(c(site_col, tag_col) %in% names(tag_history)))

  # ---- build mapping: site_code -> (occasion, occ_idx) ----
  if (is.list(locs_def)) {

    # named list: each name is an occasion label; values are site codes
    if (is.null(names(locs_def)) || any(!nzchar(names(locs_def)))) {
      stop("locs_def named list must have non-empty names (occasion labels).", call. = FALSE)
    }

    occasion <- names(locs_def)
    occ_idx  <- seq_along(locs_def)

    map <- lapply(seq_along(locs_def), function(i) {
      data.frame(
        site_code = as.character(locs_def[[i]]),
        occasion  = occasion[i],
        occ_idx   = occ_idx[i],
        stringsAsFactors = FALSE
      )
    })

    mapping <- do.call(rbind, map)

  } else if (is.character(locs_def)) {

    # simple vector: each element is both an occasion label and its unique site code
    mapping <- data.frame(
      site_code = as.character(locs_def),
      occasion  = as.character(locs_def),
      occ_idx   = seq_along(locs_def),
      stringsAsFactors = FALSE
    )

  } else {
    stop("locs_def must be a character vector OR a named list of character vectors.", call. = FALSE)
  }

  # normalize mapping
  mapping$site_code <- toupper(trimws(mapping$site_code))
  mapping$occasion  <- as.character(mapping$occasion)

  # ---- join mapping; drop or flag unknown sites ----
  df <- tag_history
  df[[site_col]] <- toupper(trimws(as.character(df[[site_col]])))
  df[[tag_col]]  <- as.character(df[[tag_col]])

  df2 <- dplyr::left_join(
    df,
    mapping,
    by = stats::setNames("site_code", site_col)
  )

  dropped <- df2[is.na(df2$occ_idx), , drop = FALSE]

  if (!keep_unknown && nrow(dropped) > 0) {
    df2 <- df2[!is.na(df2$occ_idx), , drop = FALSE]
  }

  # ---- optionally enforce downstream monotone order per tag ----
  if (enforce_order) {

    # ordering within tag: by time if available, otherwise by occ_idx
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
      dplyr::mutate(
        prev_max = cummax(dplyr::lag(as.integer(occ_idx), default = 0L))
      ) |>
      dplyr::filter(occ_idx > prev_max) |>
      dplyr::select(-prev_max) |>
      dplyr::ungroup()
  }

  # ---- reduce to (tag, occ_idx) and deduplicate ----
  surv_dat <- df2 |>
    dplyr::distinct(.data[[tag_col]], occ_idx, .keep_all = FALSE) |>
    dplyr::select(all_of(tag_col), occ_idx)

  # ---- build encounter histories ----
  n_occasions   <- length(unique(mapping$occ_idx))
  occasion_cols <- as.character(seq_len(n_occasions))

  ch_data <- surv_dat |>
    dplyr::mutate(
      detected = 1L,
      occ_idx  = as.character(occ_idx)
    ) |>
    tidyr::pivot_wider(
      id_cols     = dplyr::all_of(tag_col),
      names_from  = occ_idx,
      values_from = detected,
      values_fill = 0L
    )

  # ensure all columns exist & in order
  missing_cols <- setdiff(occasion_cols, names(ch_data))
  if (length(missing_cols)) ch_data[missing_cols] <- 0L

  ch_data <- ch_data |>
    dplyr::select(dplyr::all_of(tag_col), dplyr::all_of(occasion_cols)) |>
    dplyr::arrange(.data[[tag_col]]) |>
    dplyr::mutate(
      ch = do.call(paste0, lapply(dplyr::across(dplyr::all_of(occasion_cols)), as.character))
    ) |>
    dplyr::select(dplyr::all_of(tag_col), ch)

  # ---- collapsed frequency table ----
  ch_freq <- ch_data |>
    dplyr::count(ch, name = "freq") |>
    dplyr::arrange(dplyr::desc(freq))

  # ---- summary of dropped/unknown sites ----
  dropped_summary <- if (nrow(dropped)) {
    as.data.frame(table(dropped[[site_col]]), stringsAsFactors = FALSE) |>
      stats::setNames(c("site_code", "n")) |>
      dplyr::as_tibble()
  } else {
    tibble::tibble(site_code = character(), n = integer())
  }

  # ---- return ----
  list(
    ch_data         = ch_data,
    ch_freq         = ch_freq,
    mapping         = tibble::as_tibble(mapping),
    dropped_summary = dropped_summary
  )
}

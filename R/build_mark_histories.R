#' @title Build MARK Capture Histories from Tag Events
#'
#' @description
#' Converts tag-level observation events into capture histories compatible with
#' program MARK. Can handle multiple sites per occasion, enforce downstream order,
#' and optionally apply censoring rules that remove fish from availability after
#' a specified event.
#'
#' @details
#' If \code{censor_col} is supplied, rows where the column evaluates to \code{TRUE}
#' are treated as terminal detections. The detection at that occasion is retained
#' and encoded as \code{2} in the encounter history, while subsequent occasions are
#' encoded as \code{0} because the fish is no longer available for detection.
#'
#' This is useful in cases where a fish is detected for parameter estimation but
#' is no longer available for detection at downstream sites (e.g., removal,
#' transport, or mortality events).
#'
#' @param tag_history A data frame or tibble with at least the following columns:
#'   \describe{
#'     \item{\code{tag_code}}{Unique identifier for each tagged fish.}
#'     \item{\code{site_code}}{Detection site code.}
#'     \item{\code{event_time}}{Timestamp of the detection event (used if \code{time_col} is provided).}
#'   }
#' @param locs_def Either a character vector defining the ordered occasions
#'   (e.g., \code{c("SECTRP","ZEN",...)}), or a named list mapping one or more site codes
#'   to occasions
#'   (e.g., \code{list(LGR = c("GRJ","GRS"), Down = c("LMN","MCN","BON"))}).
#' @param site_col Character; column name in \code{tag_history} containing site codes.
#'   Default is \code{"site_code"}.
#' @param tag_col Character; column name in \code{tag_history} containing PIT tag codes.
#'   Default is \code{"tag_code"}.
#' @param time_col Character; optional column name in \code{tag_history} containing
#'   event timestamps (POSIXct or parseable) used to order events within tag.
#' @param censor_col Character; optional column name indicating whether an event
#'   is censored. Must be logical or coercible to logical. If \code{TRUE}, the fish
#'   is considered removed after that detection and is not available for subsequent
#'   occasions. The detection itself is retained. Default is \code{NULL}.
#' @param enforce_order Logical; if TRUE, only the first occurrence of each
#'   strictly increasing occasion per tag is kept. Default is TRUE.
#' @param keep_unknown Logical; whether to keep events with site codes not found in
#'   \code{locs_def} (default = \code{FALSE}).
#'
#' @return A list with four elements:
#' \itemize{
#'   \item \code{ch_data} – tibble with one row per tag and a \code{ch} column for
#'     encounter history, where \code{1} indicates detection, \code{2} indicates
#'     detection with censoring at that occasion, and \code{0} indicates no detection
#'     or not available for detection.
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
                                 censor_col = NULL,
                                 enforce_order = TRUE,
                                 keep_unknown  = FALSE) {

  stopifnot(is.data.frame(tag_history))
  stopifnot(all(c(site_col, tag_col) %in% names(tag_history)))

  if (!is.null(censor_col) && !censor_col %in% names(tag_history)) {
    stop("censor_col was supplied but is not in tag_history.", call. = FALSE)
  }

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

  if (!is.null(censor_col)) {
    df[[censor_col]] <- as.logical(df[[censor_col]])
    df[[censor_col]][is.na(df[[censor_col]])] <- FALSE
  }

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

  # ---- apply censoring: keep censored detection, drop later detections ----
  if (!is.null(censor_col)) {

    # order again within tag so first censored event is well defined
    if (!is.null(time_col) && time_col %in% names(df2)) {
      df2 <- df2[order(df2[[tag_col]], df2[[time_col]], df2$occ_idx), , drop = FALSE]
    } else {
      df2 <- df2[order(df2[[tag_col]], df2$occ_idx), , drop = FALSE]
    }

    df2 <- df2 |>
      dplyr::mutate(
        encounter = dplyr::if_else(.data[[censor_col]], 2L, 1L)
      )

    censor_df <- df2 |>
      dplyr::group_by(.data[[tag_col]]) |>
      dplyr::summarise(
        first_censor_occ = if (any(.data[[censor_col]], na.rm = TRUE)) {
          min(occ_idx[.data[[censor_col]]], na.rm = TRUE)
        } else {
          Inf
        },
        .groups = "drop"
      )

    df2 <- df2 |>
      dplyr::left_join(censor_df, by = tag_col) |>
      dplyr::filter(occ_idx <= first_censor_occ) |>
      dplyr::select(-first_censor_occ)

  } else {

    df2 <- df2 |>
      dplyr::mutate(encounter = 1L)
  }


  # ---- reduce to (tag, occ_idx) and deduplicate ----
  surv_dat <- df2 |>
    dplyr::group_by(.data[[tag_col]], occ_idx) |>
    dplyr::summarise(
      encounter = max(encounter, na.rm = TRUE),
      .groups = "drop"
    )

  # ---- build encounter histories ----
  n_occasions   <- length(unique(mapping$occ_idx))
  occasion_cols <- as.character(seq_len(n_occasions))

  ch_data <- surv_dat |>
    dplyr::mutate(
      occ_idx = as.character(occ_idx)
    ) |>
    tidyr::pivot_wider(
      id_cols     = dplyr::all_of(tag_col),
      names_from  = occ_idx,
      values_from = encounter,
      values_fill = 0L
    )

  missing_cols <- setdiff(occasion_cols, names(ch_data))
  if (length(missing_cols)) ch_data[missing_cols] <- 0L

  # ch_data <- ch_data |>
  #   dplyr::select(dplyr::all_of(tag_col), dplyr::all_of(occasion_cols)) |>
  #   dplyr::arrange(.data[[tag_col]]) |>
  #   dplyr::mutate(
  #     ch = do.call(paste0, lapply(dplyr::across(dplyr::all_of(occasion_cols)), as.character))
  #   ) |>
  #   dplyr::select(dplyr::all_of(tag_col), ch)

  ch_data <- ch_data |>
    dplyr::mutate(
      ch = do.call(paste0, lapply(dplyr::across(dplyr::all_of(occasion_cols)), as.character))
    )

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

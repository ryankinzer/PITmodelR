#' @title Build Capture Histories from Tag Events
#'
#' @description
#' Converts tag-level observation events into capture histories compatible with
#' CJS, M-array, and multistate CJS analyses. Can handle multiple sites per
#' occasion, enforce downstream order, retain individual covariates, and encode
#' censored/removal events.
#'
#' @details
#' Encounter history coding:
#' \itemize{
#'   \item \code{1}: detected and released/available for later detection
#'   \item \code{2}: detected and censored/removed after detection
#'   \item \code{0}: not detected or unavailable
#' }
#'
#' @param tag_history Data frame with one row per tag event.
#' @param locs_def Character vector of ordered occasions, or a named list mapping
#'   one or more site codes to each occasion.
#' @param site_col Character; column containing site codes. Default is
#'   \code{"site_code"}.
#' @param tag_col Character; column containing tag IDs. Default is
#'   \code{"tag_code"}.
#' @param time_col Character; optional column containing event times used for
#'   within-tag ordering.
#' @param censor_col Character; optional logical column indicating censoring or
#'   removal after detection.
#' @param covariate_cols Character vector of individual covariate columns to retain
#'   in ch_data. Default NULL keeps no covariates.
#' @param enforce_order Logical; if \code{TRUE}, only strictly increasing
#'   occasions are retained per tag. Default is \code{TRUE}.
#' @param keep_unknown Logical; whether to keep events whose site is not in
#'   \code{locs_def}. Default is \code{FALSE}.
#'
#' @return A list with \code{ch_data}, \code{ch_freq}, \code{m_array},
#'   \code{mapping}, and \code{dropped_summary}.
#'
#' @author Ryan N. Kinzer
#'
#' @export
build_capture_histories <- function(tag_history,
                                    locs_def,
                                    site_col = "site_code",
                                    tag_col  = "tag_code",
                                    time_col = NULL,
                                    censor_col = NULL,
                                    covariate_cols = NULL,
                                    enforce_order = TRUE,
                                    keep_unknown  = FALSE) {

  stopifnot(is.data.frame(tag_history))
  stopifnot(all(c(site_col, tag_col) %in% names(tag_history)))

  if (!is.null(time_col) && !time_col %in% names(tag_history)) {
    stop("time_col was supplied but is not in tag_history.", call. = FALSE)
  }

  if (!is.null(censor_col) && !censor_col %in% names(tag_history)) {
    stop("censor_col was supplied but is not in tag_history.", call. = FALSE)
  }

  if (!is.null(covariate_cols) && !all(covariate_cols %in% names(tag_history))) {
    stop("One or more covariate_cols are not in tag_history.", call. = FALSE)
  }

  if (is.list(locs_def)) {

    if (is.null(names(locs_def)) || any(!nzchar(names(locs_def)))) {
      stop("locs_def named list must have non-empty names.", call. = FALSE)
    }

    mapping <- do.call(rbind, lapply(seq_along(locs_def), function(i) {
      data.frame(
        site_code = as.character(locs_def[[i]]),
        occasion  = names(locs_def)[i],
        occ_idx   = i,
        stringsAsFactors = FALSE
      )
    }))

  } else if (is.character(locs_def)) {

    mapping <- data.frame(
      site_code = as.character(locs_def),
      occasion  = as.character(locs_def),
      occ_idx   = seq_along(locs_def),
      stringsAsFactors = FALSE
    )

  } else {
    stop("locs_def must be a character vector or named list.", call. = FALSE)
  }

  mapping$site_code <- toupper(trimws(mapping$site_code))
  mapping$occasion  <- as.character(mapping$occasion)

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

  if (!is.null(time_col) && time_col %in% names(df2)) {
    if (!inherits(df2[[time_col]], "POSIXt")) {
      suppressWarnings(df2[[time_col]] <- as.POSIXct(df2[[time_col]], tz = "UTC"))
    }

    df2 <- df2[order(df2[[tag_col]], df2[[time_col]], df2$occ_idx), , drop = FALSE]

  } else {
    df2 <- df2[order(df2[[tag_col]], df2$occ_idx), , drop = FALSE]
  }

  if (enforce_order) {
    df2 <- df2 |>
      dplyr::group_by(.data[[tag_col]]) |>
      dplyr::mutate(
        prev_max = cummax(dplyr::lag(as.integer(occ_idx), default = 0L))
      ) |>
      dplyr::filter(occ_idx > prev_max) |>
      dplyr::select(-prev_max) |>
      dplyr::ungroup()
  }

  if (!is.null(censor_col)) {

    df2 <- df2 |>
      dplyr::mutate(encounter = dplyr::if_else(.data[[censor_col]], 2L, 1L))

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
    df2 <- df2 |> dplyr::mutate(encounter = 1L)
  }

  surv_dat <- df2 |>
    dplyr::group_by(.data[[tag_col]], occ_idx) |>
    dplyr::summarise(
      encounter = max(encounter, na.rm = TRUE),
      .groups = "drop"
    )

  n_occasions <- length(unique(mapping$occ_idx))
  occasion_cols <- as.character(seq_len(n_occasions))

  ch_wide <- surv_dat |>
    dplyr::mutate(occ_idx = as.character(occ_idx)) |>
    tidyr::pivot_wider(
      id_cols = dplyr::all_of(tag_col),
      names_from = occ_idx,
      values_from = encounter,
      values_fill = 0L
    )

  missing_cols <- setdiff(occasion_cols, names(ch_wide))
  if (length(missing_cols)) ch_wide[missing_cols] <- 0L

  ch_data <- ch_wide |>
    dplyr::select(dplyr::all_of(tag_col), dplyr::all_of(occasion_cols)) |>
    dplyr::arrange(.data[[tag_col]]) |>
    dplyr::mutate(
      ch = do.call(
        paste0,
        lapply(dplyr::across(dplyr::all_of(occasion_cols)), as.character)
      )
    ) |>
    dplyr::select(dplyr::all_of(tag_col), ch)

  event_cols <- unique(c(site_col, time_col, censor_col, "occ_idx", "occasion", "encounter"))
  candidate_covars <- setdiff(names(tag_history), c(event_cols, site_col))

  if (is.null(covariate_cols)) {
    covariate_cols <- character(0)
  }

  # retain requested covariates only
  if (length(covariate_cols) > 0) {

    covars <- tag_history |>
      dplyr::select(dplyr::all_of(c(tag_col, covariate_cols))) |>
      dplyr::distinct()

    covar_n <- covars |>
      dplyr::count(.data[[tag_col]]) |>
      dplyr::filter(n > 1)

    if (nrow(covar_n)) {
      warning(
        "Some tags have multiple covariate values. Keeping first observed value per tag.",
        call. = FALSE
      )

      covars <- covars |>
        dplyr::group_by(.data[[tag_col]]) |>
        dplyr::slice(1) |>
        dplyr::ungroup()
    }

    ch_data <- dplyr::left_join(ch_data, covars, by = tag_col)
  }

  names(ch_data)[names(ch_data) == tag_col] <- "tag_code"

  ch_freq <- ch_data |>
    dplyr::count(ch, name = "freq") |>
    dplyr::arrange(dplyr::desc(freq))

  dropped_summary <- if (nrow(dropped)) {
    as.data.frame(table(dropped[[site_col]]), stringsAsFactors = FALSE) |>
      stats::setNames(c("site_code", "n")) |>
      dplyr::as_tibble()
  } else {
    tibble::tibble(site_code = character(), n = integer())
  }

  occasion_labels <- unique(mapping$occasion[order(mapping$occ_idx)])

  m_array <- build_marray(
    ch_data = ch_data,
    locs_def = occasion_labels
  )

  list(
    ch_data = ch_data,
    ch_freq = ch_freq,
    m_array = m_array,
    mapping = tibble::as_tibble(mapping),
    dropped_summary = dropped_summary
  )
}

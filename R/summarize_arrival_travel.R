#' @title Summarize Arrival and Travel Times Between Locations
#'
#' @description
#' Given raw tag histories e.g., from `get_batch_tag_histories()` and user-defined sequence of
#' detection locations or occasions, summarizes first-arrival and travel times for tagged fish.
#' Calculates per-tag first arrival and travel times, as well as quantile summaries for each
#' occassion and migration "leg".
#'
#' @param tag_history A data frame or tibble with at least the following columns:
#'   \describe{
#'     \item{\code{tag_code}}{Unique identifier for each tagged fish.}
#'     \item{\code{site_code}}{Detection site code.}
#'     \item{\code{event_time}}{Timestamp of the detection event.}
#'   }
#' @param locs_def Either a character vector defining the ordered occasions
#'   (e.g., \code{c("SECTRP","ZEN",...)}), or a named list mapping one or more site codes
#'   to occasions
#'   (e.g., \code{list(LGR = c("GRJ","GRS"), Down = c("LMN","MCN","BON"))}).
#' @param site_col Column name containing site codes (default = \code{"site_code"}).
#' @param tag_col Column name containing tag IDs (default = \code{"tag_code"}).
#' @param time_col Column name with event timestamps (default = \code{"event_time"}).
#' @param tz Time zone used when parsing \code{time_col} if it is character (default = \code{"America/Los_Angeles"}, Pacific Standard).
#' @param keep_unknown Logical; whether to keep events with site codes not found in
#'   \code{locs_def} (default = \code{FALSE}).
#'
#' @return A list with the following elements:
#' \describe{
#'   \item{\code{arrivals_long}}{First-arrival time per tag × occasion (long format).}
#'   \item{\code{arrivals_wide}}{First-arrival time per tag (wide format).}
#'   \item{\code{travel_long}}{Travel time (days) per tag and leg (\eqn{occasion_i \to occasion_{i+1}}).}
#'   \item{\code{occasion_summary}}{Quantile summaries of arrival times by occasion.}
#'   \item{\code{leg_summary}}{Quantile summaries of travel times by migration leg.}
#'   \item{\code{mapping}}{Mapping of \code{site_code} to occasion and occasion index.}
#'   \item{\code{dropped_summary}}{Counts of unknown sites removed (if any).}
#' }
#'
#' @author Ryan Kinzer
#'
#' @seealso [plot_arrival_travel()]
#'
#' @export

summarize_arrival_travel <- function(tag_history,
                                     locs_def,
                                     site_col = "site_code",
                                     tag_col  = "tag_code",
                                     time_col = "event_date",
                                     tz = "America/Los_Angeles",
                                     keep_unknown = FALSE) {
  # --- validate inputs ---
  stopifnot(is.data.frame(tag_history))
  stopifnot(all(c(site_col, tag_col, time_col) %in% names(tag_history)))

  # --- build site -> occasion mapping ---
  if (is.list(locs_def)) {
    if (is.null(names(locs_def)) || any(!nzchar(names(locs_def)))) {
      stop("locs_def named list must have non-empty names (occasion labels).", call. = FALSE)
    }

    occ_names <- names(locs_def)
    mapping <- do.call(rbind, lapply(seq_along(locs_def), function(i) {
      data.frame(
        site_code = as.character(locs_def[[i]]),
        occasion  = occ_names[i],
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
    stop("locs_def must be a character vector OR a named list of character vectors.", call. = FALSE)
  }

  mapping$site_code <- toupper(trimws(mapping$site_code))

  # --- normalize tag_history and join mapping ---
  df <- tag_history
  df[[site_col]] <- toupper(trimws(as.character(df[[site_col]])))
  df[[tag_col]]  <- as.character(df[[tag_col]])

  # parse time if not already POSIXt
  if (!inherits(df[[time_col]], "POSIXt")) {
    suppressWarnings({
      dt <- as.POSIXct(df[[time_col]], tz = tz, format = "%Y-%m-%dT%H:%M:%S")
      na <- is.na(dt)
      if (any(na)) {
        dt2 <- as.POSIXct(df[[time_col]][na], tz = tz, format = "%Y-%m-%d %H:%M:%S")
        dt[na] <- dt2
      }
      df[[time_col]] <- dt
    })
  }

  df2 <- merge(df, mapping,
               by.x = site_col, by.y = "site_code",
               all.x = TRUE, all.y = FALSE, sort = FALSE)

  dropped <- df2[is.na(df2$occ_idx), , drop = FALSE]
  if (!keep_unknown && nrow(dropped)) {
    df2 <- df2[!is.na(df2$occ_idx), , drop = FALSE]
  }

  # --- first arrival per tag × occasion ---
  keep_cols <- c(tag_col, "occasion", "occ_idx", time_col)
  df2 <- df2[stats::complete.cases(df2[, keep_cols]), keep_cols, drop = FALSE]

  o <- order(df2[[tag_col]], df2$occ_idx, df2[[time_col]])
  df2 <- df2[o, ]

  key <- paste(df2[[tag_col]], df2$occ_idx, sep = "\r")
  first_idx <- !duplicated(key)
  arrivals_long <- df2[first_idx, ]
  rownames(arrivals_long) <- NULL

  # --- wide table of first arrivals per tag ---
  arrivals_wide <- reshape(
    arrivals_long[, c(tag_col, "occasion", time_col)],
    idvar = tag_col,
    timevar = "occasion",
    direction = "wide"
  )

  names(arrivals_wide) <- sub(paste0("^", time_col, "\\."), "", names(arrivals_wide), perl = TRUE)

  # --- get order of occasions ---
  occ_order <- unique(mapping[, c("occasion", "occ_idx")])
  occ_order <- occ_order[order(occ_order$occ_idx), , drop = FALSE]

  # reorder columns to match mapping order
  wide_cols <- c(tag_col, occ_order$occasion)
  arrivals_wide <- arrivals_wide[, intersect(wide_cols, names(arrivals_wide)), drop = FALSE]

  # --- travel times between consecutive occasions (days) ---
  split_by_tag <- split(arrivals_long, arrivals_long[[tag_col]])
  travel_long <- do.call(rbind, lapply(names(split_by_tag), function(tg) {
    xx <- split_by_tag[[tg]][order(split_by_tag[[tg]]$occ_idx), ]
    if (nrow(xx) < 2) return(NULL)

    t1 <- xx[[time_col]][-nrow(xx)]
    t2 <- xx[[time_col]][-1]
    leg <- paste(xx$occasion[-nrow(xx)], "->", xx$occasion[-1])

    data.frame(
      tag_code = tg,
      from_idx = xx$occ_idx[-nrow(xx)],
      to_idx   = xx$occ_idx[-1],
      leg      = leg,
      travel_days = as.numeric(difftime(t2, t1, units = "days")),
      stringsAsFactors = FALSE
    )
  }))

  if (is.null(travel_long)) {
    travel_long <- data.frame(
      tag_code = character(),
      from_idx = integer(),
      to_idx   = integer(),
      leg      = character(),
      travel_days = numeric(),
      stringsAsFactors = FALSE
    )
  }

  # --- summaries (robust quantiles) ---
  qfun <- function(x) stats::quantile(x, probs = c(0.1, 0.25, 0.5, 0.75, 0.9), na.rm = TRUE, names = FALSE)

  # arrival summaries by occasion
  occasion_summary <- do.call(rbind, lapply(split(arrivals_long, arrivals_long$occasion), function(dd) {
    times <- dd[[time_col]]
    n <- length(times)

    data.frame(
      occasion = dd$occasion[1],
      n = n,
      p10 = as.POSIXct(stats::quantile(as.numeric(times), 0.10, na.rm = TRUE), origin = "1970-01-01", tz = tz),
      p25 = as.POSIXct(stats::quantile(as.numeric(times), 0.25, na.rm = TRUE), origin = "1970-01-01", tz = tz),
      p50 = as.POSIXct(stats::quantile(as.numeric(times), 0.50, na.rm = TRUE), origin = "1970-01-01", tz = tz),
      p75 = as.POSIXct(stats::quantile(as.numeric(times), 0.75, na.rm = TRUE), origin = "1970-01-01", tz = tz),
      p90 = as.POSIXct(stats::quantile(as.numeric(times), 0.90, na.rm = TRUE), origin = "1970-01-01", tz = tz),
      stringsAsFactors = FALSE
    )
  }))

  if (is.null(occasion_summary)) {
    occasion_summary <- data.frame(
      occasion = character(), n = integer(),
      p10 = as.POSIXct(character()), p25 = as.POSIXct(character()),
      p50 = as.POSIXct(character()), p75 = as.POSIXct(character()),
      p90 = as.POSIXct(character()), stringsAsFactors = FALSE
    )
  }

  # travel-time summaries by leg
  leg_summary <- do.call(rbind, lapply(split(travel_long, travel_long$leg), function(dd) {
    if (!nrow(dd)) return(NULL)
    qs <- qfun(dd$travel_days)

    data.frame(
      leg = dd$leg[1], n = nrow(dd),
      p10 = qs[1], p25 = qs[2], median = qs[3],
      p75 = qs[4], p90 = qs[5],
      stringsAsFactors = FALSE
    )
  }))

  if (is.null(leg_summary)) {
    leg_summary <- data.frame(
      leg = character(), n = integer(),
      p10 = numeric(), p25 = numeric(), median = numeric(),
      p75 = numeric(), p90 = numeric(), stringsAsFactors = FALSE
    )
  }

  # --- dropped sites summary ---
  dropped_summary <- if (nrow(dropped)) {
    as.data.frame(table(dropped[[site_col]]), stringsAsFactors = FALSE)
  } else {
    data.frame(Var1 = character(), Freq = integer(), stringsAsFactors = FALSE)
  }
  names(dropped_summary) <- c("site_code", "n")

  # sort to match order of occasions
  arrivals_long$occasion <- factor(arrivals_long$occasion, levels = occ_order$occasion)
  travel_long$leg <- factor(
    travel_long$leg,
    levels = apply(combn(occ_order$occasion, 2), 2, function(x) paste(x[1], "->", x[2]))
    #levels = paste(head(occ_order$occasion, -1), "->", tail(occ_order$occasion, -1))
  )

  # --- return results ---
  list(
    arrivals_long     = arrivals_long,
    arrivals_wide     = arrivals_wide,
    travel_long       = travel_long,
    occasion_summary  = occasion_summary[order(match(occasion_summary$occasion, occ_order$occasion)), ],
    leg_summary       = leg_summary[order(match(
      leg_summary$leg,
      paste(head(occ_order$occasion, -1), "->", tail(occ_order$occasion, -1))
    )), ],
    mapping           = mapping,
    dropped_summary   = dropped_summary
  )
}

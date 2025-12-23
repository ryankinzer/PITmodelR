#' Summarize arrival times and travel times between locations
#'
#' Given raw tag events and a survival-location definition, returns:
#' - First-arrival time at each occasion per tag
#' - Travel time between consecutive occasions per tag
#' - Per-occasion arrival summaries (n, quantiles)
#' - Per-leg travel-time summaries (n, quantiles)
#'
#' @param tag_history data.frame/tibble with at least `tag_code`, `site_code`, `event_time`.
#' @param locs_def Either a character vector of ordered occasions (e.g., c("SECTRP","ZEN",...)),
#'   or a named list mapping occasion -> one or more site codes
#'   (e.g., list(LGR=c("GRJ","GRS","LGR"), Down=c("LMN","MCN","BON"))).
#' @param site_col Column name containing site codes (default "site_code").
#' @param tag_col  Column name containing tag IDs   (default "tag_code").
#' @param time_col Column name with event timestamps (default "event_time").
#' @param tz       Time zone used when parsing `time_col` if it is character (default "UTC").
#' @param keep_unknown logical; keep events whose site_code is not in `locs_def`? (default FALSE).
#' @return A list with elements:
#'   - arrivals_long:  first-arrival time per tag x occasion (long)
#'   - arrivals_wide:  first-arrival time per tag (wide by occasion)
#'   - travel_long:    travel time (days) per tag and leg (from occasion i to occasion i+1)
#'   - occasion_summary: arrival-time quantiles by occasion
#'   - leg_summary:       travel-time quantiles by leg
#'   - mapping: site_code -> occasion, occ_idx
#'   - dropped_summary: counts of unknown sites (if removed)
#' @export
summarize_arrival_travel <- function(tag_history,
                                     locs_def,
                                     site_col = "site_code",
                                     tag_col  = "tag_code",
                                     time_col = "event_time",
                                     tz = "UTC",
                                     keep_unknown = FALSE) {
  stopifnot(is.data.frame(tag_history))
  stopifnot(all(c(site_col, tag_col, time_col) %in% names(tag_history)))

  # ---- build site->occasion mapping (same convention as earlier helpers) ----
  if (is.list(locs_def)) {
    if (is.null(names(locs_def)) || any(!nzchar(names(locs_def)))) {
      stop("locs_def named list must have non-empty names (occasion labels).", call. = FALSE)
    }
    occ_names <- names(locs_def)
    mapping <- do.call(rbind, lapply(seq_along(locs_def), function(i) {
      data.frame(site_code = as.character(locs_def[[i]]),
                 occasion  = occ_names[i],
                 occ_idx   = i,
                 stringsAsFactors = FALSE)
    }))
  } else if (is.character(locs_def)) {
    mapping <- data.frame(site_code = as.character(locs_def),
                          occasion  = as.character(locs_def),
                          occ_idx   = seq_along(locs_def),
                          stringsAsFactors = FALSE)
  } else {
    stop("locs_def must be a character vector OR a named list of character vectors.", call. = FALSE)
  }
  mapping$site_code <- toupper(trimws(mapping$site_code))

  # ---- normalize and join ----
  df <- tag_history
  df[[site_col]] <- toupper(trimws(as.character(df[[site_col]])))
  df[[tag_col]]  <- as.character(df[[tag_col]])

  # parse time if not POSIXt
  if (!inherits(df[[time_col]], "POSIXt")) {
    # Accept ISO "YYYY-MM-DDTHH:MM:SS" or "YYYY-MM-DD HH:MM:SS"
    # If parsing fails, may produce NA; we keep them but they'll drop later.
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

  # ---- first arrival per tag x occasion ----
  # Keep earliest timestamp per (tag, occasion)
  # (If some tags have NA times, they'll be removed when taking min)
  keep_cols <- c(tag_col, "occasion", "occ_idx", time_col)
  df2 <- df2[stats::complete.cases(df2[, keep_cols]), keep_cols, drop = FALSE]

  # order to ensure stable mins
  o <- order(df2[[tag_col]], df2$occ_idx, df2[[time_col]])
  df2 <- df2[o, ]

  # get first occurrence per (tag, occ)
  key <- paste(df2[[tag_col]], df2$occ_idx, sep = "\r")
  first_idx <- !duplicated(key)
  arrivals_long <- df2[first_idx, ]
  rownames(arrivals_long) <- NULL

  # wide table of first arrivals per tag
  arrivals_wide <- reshape(arrivals_long[, c(tag_col, "occasion", time_col)],
                           idvar = tag_col, timevar = "occasion",
                           direction = "wide")
  # clean wide names: event_time.SECTION -> occ label
  names(arrivals_wide) <- sub(paste0("^", time_col, "\\."), "", names(arrivals_wide), perl = TRUE)

  # ---- travel times between consecutive occasions (in days) ----
  # For each tag, order by occ_idx and diff successive first-arrival times
  split_by_tag <- split(arrivals_long, arrivals_long[[tag_col]])
  travel_long <- do.call(rbind, lapply(names(split_by_tag), function(tg) {
    xx <- split_by_tag[[tg]][order(split_by_tag[[tg]]$occ_idx), ]
    if (nrow(xx) < 2) return(NULL)
    t1 <- xx[[time_col]][-nrow(xx)]
    t2 <- xx[[time_col]][-1]
    leg <- paste(xx$occasion[-nrow(xx)], "->", xx$occasion[-1])
    data.frame(tag_code = tg,
               from_idx = xx$occ_idx[-nrow(xx)],
               to_idx   = xx$occ_idx[-1],
               leg      = leg,
               travel_days = as.numeric(difftime(t2, t1, units = "days")),
               stringsAsFactors = FALSE)
  }))
  if (is.null(travel_long)) {
    travel_long <- data.frame(tag_code = character(), from_idx = integer(),
                              to_idx = integer(), leg = character(),
                              travel_days = numeric(), stringsAsFactors = FALSE)
  }

  # ---- summaries (robust quantiles) ----
  qfun <- function(x) stats::quantile(x, probs = c(0.1, 0.25, 0.5, 0.75, 0.9), na.rm = TRUE, names = FALSE)

  # arrival: convert arrival time to day-of-year or keep datetime summaries?
  # We'll keep datetime summaries via ISO string medians; plus n.
  # (Median of POSIXct via numeric; report as POSIXct)
  occasion_summary <- do.call(rbind, lapply(split(arrivals_long, arrivals_long$occasion), function(dd) {
    times <- dd[[time_col]]
    n <- length(times)
    out <- data.frame(
      occasion = dd$occasion[1],
      n = n,
      p10 = as.POSIXct(stats::quantile(as.numeric(times), probs = 0.10, na.rm = TRUE), origin = "1970-01-01", tz = tz),
      p25 = as.POSIXct(stats::quantile(as.numeric(times), probs = 0.25, na.rm = TRUE), origin = "1970-01-01", tz = tz),
      p50 = as.POSIXct(stats::quantile(as.numeric(times), probs = 0.50, na.rm = TRUE), origin = "1970-01-01", tz = tz),
      p75 = as.POSIXct(stats::quantile(as.numeric(times), probs = 0.75, na.rm = TRUE), origin = "1970-01-01", tz = tz),
      p90 = as.POSIXct(stats::quantile(as.numeric(times), probs = 0.90, na.rm = TRUE), origin = "1970-01-01", tz = tz),
      stringsAsFactors = FALSE
    )
    out
  }))
  if (is.null(occasion_summary)) {
    occasion_summary <- data.frame(occasion = character(), n = integer(),
                                   p10 = as.POSIXct(character()), p25 = as.POSIXct(character()),
                                   p50 = as.POSIXct(character()), p75 = as.POSIXct(character()),
                                   p90 = as.POSIXct(character()),
                                   stringsAsFactors = FALSE)
  }

  # travel-time summaries by leg
  leg_summary <- do.call(rbind, lapply(split(travel_long, travel_long$leg), function(dd) {
    if (!nrow(dd)) return(NULL)
    qs <- qfun(dd$travel_days)
    data.frame(leg = dd$leg[1], n = nrow(dd),
               p10 = qs[1], p25 = qs[2], median = qs[3], p75 = qs[4], p90 = qs[5],
               stringsAsFactors = FALSE)
  }))
  if (is.null(leg_summary)) {
    leg_summary <- data.frame(leg = character(), n = integer(),
                              p10 = numeric(), p25 = numeric(), median = numeric(),
                              p75 = numeric(), p90 = numeric(), stringsAsFactors = FALSE)
  }

  # dropped sites
  dropped_summary <- if (nrow(dropped)) {
    as.data.frame(table(dropped[[site_col]]), stringsAsFactors = FALSE)
  } else data.frame(Var1 = character(), Freq = integer(), stringsAsFactors = FALSE)
  names(dropped_summary) <- c("site_code","n")

  # wide order by original occ order
  occ_order <- unique(mapping[, c("occasion","occ_idx")])
  occ_order <- occ_order[order(occ_order$occ_idx), , drop = FALSE]
  arrivals_long$occasion <- factor(arrivals_long$occasion, levels = occ_order$occasion)
  travel_long$leg <- factor(travel_long$leg,
                            levels = paste(head(occ_order$occasion, -1), "->", tail(occ_order$occasion, -1)))

  list(
    arrivals_long   = arrivals_long,
    arrivals_wide   = arrivals_wide,
    travel_long     = travel_long,
    occasion_summary = occasion_summary[order(match(occasion_summary$occasion, occ_order$occasion)), ],
    leg_summary      = leg_summary[order(match(leg_summary$leg,
                                               paste(head(occ_order$occasion,-1),"->",tail(occ_order$occasion,-1)))), ],
    mapping          = mapping,
    dropped_summary  = dropped_summary
  )
}

#' Plot arrival timing (ECDF) and travel times by leg
#'
#' @param timing_list Result from `summarize_arrival_travel()`
#' @param tz Label/timezone for axis formatting (default "UTC")
#' @return A list with `$arrival_ecdf` and `$travel_time` plot objects.
#'         If ggplot2 is not available, returns functions that draw base plots.
#' @export
plot_arrival_travel <- function(timing_list, tz = "UTC") {
  arrivals <- timing_list$arrivals_long
  travel   <- timing_list$travel_long

  # -------- detect the arrivals time column robustly --------
  time_col <- NULL
  if (nrow(arrivals)) {
    posix_cols <- names(arrivals)[vapply(arrivals, function(x) inherits(x, "POSIXt"), logical(1))]
    if (length(posix_cols)) {
      time_col <- posix_cols[1]
    } else if ("event_time" %in% names(arrivals)) {
      time_col <- "event_time"
    } else {
      stop("plot_arrival_travel(): could not find a POSIXct time column in arrivals_long. ",
           "Make sure summarize_arrival_travel() parsed time to POSIXct or the column is named 'event_time'.",
           call. = FALSE)
    }
  }

  # -------- Arrival ECDF by occasion --------
  arrival_plot <- NULL
  if (nrow(arrivals)) {
    if (requireNamespace("ggplot2", quietly = TRUE)) {
      arrival_plot <- ggplot2::ggplot(arrivals, ggplot2::aes(x = .data[[time_col]])) +
        ggplot2::stat_ecdf(geom = "step") +
        ggplot2::facet_wrap(~ occasion, scales = "free_x") +
        ggplot2::labs(x = paste0("Arrival time (", tz, ")"),
                      y = "Cumulative proportion",
                      title = "Arrival timing by occasion") +
        ggplot2::theme_minimal()
    } else {
      df <- arrivals
      arrival_plot <- function() {
        op <- par(no.readonly = TRUE); on.exit(par(op))
        occs <- levels(df$occasion)
        if (is.null(occs)) occs <- unique(df$occasion)
        n <- length(occs); nr <- ceiling(sqrt(n)); nc <- ceiling(n / nr)
        par(mfrow = c(max(1, nr), max(1, nc)))
        for (oc in occs) {
          xx <- sort(df[df$occasion == oc, time_col, drop = TRUE])
          if (length(xx)) {
            yy <- seq_along(xx) / length(xx)
            plot(xx, yy, type = "s",
                 xlab = paste0("Arrival time (", tz, ")"),
                 ylab = "ECDF", main = oc)
          } else {
            plot(NA, NA, xlab = "", ylab = "", main = oc)
            text(0.5, 0.5, "no data")
          }
        }
        invisible(NULL)
      }
    }
  }

  # -------- Travel time box/violin by leg --------
  travel_plot <- NULL
  if (nrow(travel)) {
    if (requireNamespace("ggplot2", quietly = TRUE)) {
      travel_plot <- ggplot2::ggplot(travel, ggplot2::aes(x = leg, y = travel_days)) +
        ggplot2::geom_violin(trim = TRUE, fill = NA) +
        ggplot2::geom_boxplot(width = 0.2, outlier.size = 0.8) +
        ggplot2::labs(x = "Leg", y = "Travel time (days)",
                      title = "Travel time between occasions") +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
    } else {
      df <- travel
      travel_plot <- function() {
        op <- par(no.readonly = TRUE); on.exit(par(op))
        boxplot(travel_days ~ leg, data = df, las = 2, ylab = "Travel time (days)",
                main = "Travel time between occasions")
        invisible(NULL)
      }
    }
  }

  list(arrival_ecdf = arrival_plot, travel_time = travel_plot)
}

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

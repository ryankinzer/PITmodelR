#' @title Load interrogation site configuration file.
#'
#' @description
#' Download site configuration file from DART (https://www.cbr.washington.edu/paramest/docs/pitpro/updates/sites_config.txt) and format as a data frame. The function was originally written by Ryan Vosbigian and contained in the 'https://github.com/ryanvosbigian/space4time' R package.
#'
#' @param configDate last day of configuration
#'
#' @return A tibble of site observations.
#'
#' @author Ryan N. Kinzer
#'
#' @export

DART_site_config <- function(configDate = Sys.Date()){

  DART_config <- "https://www.cbr.washington.edu/paramest/docs/pitpro/updates/sites_config.txt"

  lines <- tryCatch(
    readLines(DART_config),
    error = function(err) stop("DART site configuration file failed: ", DART_config)
  )

  # removes some special characters
  lines <- iconv(lines, from = "latin1", to = "UTF-8", sub = "")

  message("Obtained the PTAGIS site configuration formatted by DART from:\n", DART_config)

  parse_date <- function(x) {
    ifelse(
      x == "Present",
      as.character(configDate),
      as.character(as.Date(x, format = "%d-%b-%y"))
    ) |>
      as.Date()
  }

  parse_array_lines <- function(x, obs_site, date_start, date_end, exception = FALSE) {
    data.frame(
      obs_site = obs_site,
      antenna_name = stringr::str_split_i(x, " : ", 3),
      antenna_id = stringr::str_split_i(x, " : ", 4),
      date_start = as.Date(date_start),
      date_end = as.Date(date_end),
      release_code = gsub(" ", "", stringr::str_split_i(x, ": ", 1)),
      life_stage = gsub(" ", "", stringr::str_split_i(x, ":", 2)),
      antenna_order = gsub(" ", "", stringr::str_split_i(x, ":", 3)),
      exception = exception,
      stringsAsFactors = FALSE
    )
  }

  # isolate detector configuration section
  start_line <- which(grepl("# Detector Configuration", lines)) + 2
  end_line   <- which(grepl("# Interrogation Site Configuration", lines)) - 2

  detector_lines <- lines[start_line:end_line]

  code_start <- grep("code:", detector_lines)
  code_end   <- c(code_start[-1] - 1, length(detector_lines))

  parsed_df <- do.call(rbind, lapply(seq_along(code_start), function(i) {

    code_lines <- detector_lines[code_start[i]:code_end[i]]
    obs_site <- gsub("code: ", "", code_lines[1])

    close_paren <- grep("[}]$", code_lines)
    range_lines <- grep("range:", code_lines)

    range_df <- do.call(rbind, lapply(range_lines, function(r) {

      date_range <- strsplit(gsub("^.*range: ", "", code_lines[r]), "\\s+")[[1]]

      date_start <- parse_date(date_range[1])
      date_end   <- parse_date(date_range[2])

      next_close <- min(close_paren[close_paren > r])
      array_lines <- code_lines[(r + 2):(next_close - 1)]

      parse_array_lines(
        x = array_lines,
        obs_site = obs_site,
        date_start = date_start,
        date_end = date_end,
        exception = FALSE
      )
    }))

    exception_lines <- code_lines[grepl("exception", code_lines)]

    exception_df <- if (length(exception_lines)) {

      exc <- gsub("^.*exception: ", "", exception_lines)

      date_start <- parse_date(stringr::str_split_i(exc, "\\s+", 1))
      date_end   <- parse_date(stringr::str_split_i(exc, "\\s+", 2))

      array_lines <- gsub(".*[{] | [}]", "", exc)

      parse_array_lines(
        x = array_lines,
        obs_site = obs_site,
        date_start = date_start,
        date_end = date_end,
        exception = TRUE
      )

    } else {
      NULL
    }

    rbind(range_df, exception_df)
  }))

  disposition_map <- c(
    R = "Returned to River",
    U = "Unknown",
    B = "Bypass",
    S = "Sampled",
    T = "Transported",
    H = "Held",
    X = "Unknown"
  )

  censored_map <- c(
    R = FALSE,
    U = TRUE,
    B = TRUE,
    S = TRUE,
    T = FALSE,
    H = TRUE,
    X = TRUE
  )

  parsed_df$disposition <- disposition_map[parsed_df$release_code]
  parsed_df$censored    <- censored_map[parsed_df$release_code]
  parsed_df$release_code <- NULL

  # split antenna_id into one row per antenna
  split_antennas <- strsplit(parsed_df$antenna_id, "\\s+")

  site_config <- parsed_df[
    rep(seq_len(nrow(parsed_df)), lengths(split_antennas)),
  ]

  site_config$antenna_id <- unlist(split_antennas)

  # keep exception rows when duplicate config records exist
  site_config <- site_config[order(site_config$exception, decreasing = TRUE), ]

  group_cols <- names(site_config)[
    match("obs_site", names(site_config)):match("date_end", names(site_config))
  ]

  site_config <- site_config[!duplicated(site_config[group_cols]), ]
  row.names(site_config) <- NULL

  return(site_config)
}

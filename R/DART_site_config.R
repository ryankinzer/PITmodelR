#' @title Load PTAGIS interrogation site configuration
#'
#' @description
#' Download site configuration file from DART (https://www.cbr.washington.edu/paramest/docs/pitpro/updates/sites_config.txt) and format as a data frame.
#'
#' @param configDate last day of configuration
#'
#' @return A tibble of site observations.
#'
#' @author Ryan Vosbigian
#'
#' @examples
#' \dontrun{
#' # recommended: store your key once per session
#' Sys.setenv(PTAGIS_API_KEY = "YOUR-KEY-HERE")
#' get_tag_history(tag_code = "384.1B79726A98")
#'
#' # or pass the key manually, and reduce fields
#' get_tag_history(api_key = "YOUR-KEY-HERE", tag_code = "384.1B79726A98",
#'                 fields = c("tag_code", "site_code", "event_type", "event_date"))
#' }
#'
#' @export

DART_site_config <- function(configDate = Sys.Date()) {

  DART_config <- 'https://www.cbr.washington.edu/paramest/docs/pitpro/updates/sites_config.txt'

  lines <- tryCatch(readLines(DART_config),
                    error = function(err) stop(paste0("DART site configuration file failed: ",DART_config)))

  message(paste0("Obtained the PTAGIS site configuration formatted by DART from:\n",DART_config))

  parsed_df <- data.frame(sitecode = as.character(NA),
                          arrayname = as.character(NA),
                          arraycodes = as.character(NA),
                          date_start = as.Date(NA),
                          date_end = as.Date(NA),
                          releasecode1 = as.character(NA),
                          releasecode2 = as.character(NA),
                          releasecode3 = as.character(NA),
                          exception = as.logical(NA),
                          stringsAsFactors = FALSE)[0,]

  # Find the start and end of the desired section
  suppressWarnings(start_line <- which(grepl("# Detector Configuration", lines)) + 2)
  suppressWarnings(end_line <- which(grepl("# Interrogation Site Configuration", lines)) - 2)

  detector_config_lines <- lines[start_line:end_line]

  code_start <- grep("code:",detector_config_lines)
  code_end <- c(code_start[-1]-1,length(detector_config_lines))


  for (c in 1:length(code_start)) {



    tmp_code_lines <- detector_config_lines[code_start[c]:code_end[c]]
    tmp_openparanth <- grep("[{]$",tmp_code_lines)
    tmp_cloparanth <- grep("[}]$",tmp_code_lines)
    tmp_range <- grep("range:",tmp_code_lines)



    # or length tmp_range
    for (r in 1:(length(tmp_range))) {
      # Present <- Sys.Date()


      # tmp_code_lines[tmp_range[p-1]]
      tmpdate_range <- gsub("^.*range: ","",tmp_code_lines[tmp_range[r]])

      tmp_startdate <- as.Date(stringr::str_split_i(tmpdate_range," ",1),format = "%d-%b-%y")

      #
      tmp_enddate <- ifelse(stringr::str_split_i(tmpdate_range," ",2) == "Present",as.Date(configDate),as.Date(stringr::str_split_i(tmpdate_range," ",2),format = "%d-%b-%y"))

      next_closed_paran <- min(tmp_cloparanth[which(tmp_cloparanth-tmp_range[r] > 0)])

      tmp_array_lines <- tmp_code_lines[seq(tmp_range[r] + 2,next_closed_paran-1)]

      arraynames <- stringr::str_split_i(tmp_array_lines,pattern = " : ",i = 3)
      arraycodes <- stringr::str_split_i(tmp_array_lines,pattern = " : ",i = 4)

      releasecode1 <- stringr::str_split_i(tmp_array_lines,pattern = ": ",i = 1)
      releasecode1 <- gsub(" ","",releasecode1)

      releasecode2 <- stringr::str_split_i(tmp_array_lines,pattern = ":",i = 2)

      releasecode2 <- gsub(" ","",releasecode2)

      releasecode3 <- stringr::str_split_i(tmp_array_lines,pattern = ":",i = 3)
      releasecode3 <- gsub(" ","",releasecode3)

      new_row <- data.frame(sitecode = gsub("code: ","",tmp_code_lines[1]),
                            arrayname = arraynames,
                            arraycodes = arraycodes,
                            date_start = tmp_startdate,
                            date_end =as.Date(tmp_enddate),
                            releasecode1 = releasecode1,
                            releasecode2 = releasecode2,
                            releasecode3 = releasecode3,
                            exception = FALSE,
                            stringsAsFactors = FALSE)

      parsed_df <- rbind(parsed_df, new_row)
    }

    # deal with exceptions:
    if (sum(grepl("exception",tmp_code_lines)) > 0) {
      # stop()
      tmp_exception_lines <- tmp_code_lines[grepl("exception",tmp_code_lines)]

      format_tmp_exception_lines <- gsub("^.*exception: ","",tmp_exception_lines)

      tmp_date_start <- stringr::str_split_i(format_tmp_exception_lines," ",1)
      tmp_date_end <- stringr::str_split_i(format_tmp_exception_lines," ",2)


      tmp_date_start <- as.Date(tmp_date_start,format = "%d-%b-%y")
      tmp_date_end <- as.Date(ifelse(tmp_date_end == "Present",as.Date(configDate),as.Date(tmp_date_end,format = "%d-%b-%y")))

      format2_tmp_exc_lines <- gsub(".*[{] | [}]","",format_tmp_exception_lines)

      arraynames <- stringr::str_split_i(format2_tmp_exc_lines,pattern = " : ",i = 3)
      arraycodes <- stringr::str_split_i(format2_tmp_exc_lines,pattern = " : ",i = 4)

      releasecode1 <- stringr::str_split_i(format2_tmp_exc_lines,pattern = ": ",i = 1)

      releasecode2 <- stringr::str_split_i(format2_tmp_exc_lines,pattern = ":",i = 2)

      releasecode2 <- gsub(" ","",releasecode2)

      releasecode3 <- stringr::str_split_i(format2_tmp_exc_lines,pattern = ":",i = 3)

      releasecode3 <- gsub(" ","",releasecode3)

      new_row <- data.frame(sitecode = gsub("code: ","",tmp_code_lines[1]),
                            arrayname = arraynames,
                            arraycodes = arraycodes,
                            date_start = tmp_date_start,
                            date_end =tmp_date_end,
                            releasecode1 = releasecode1,
                            releasecode2 = releasecode2,
                            releasecode3 = releasecode3,
                            exception = TRUE,
                            stringsAsFactors = FALSE)

      parsed_df <- rbind(parsed_df, new_row)


    }


  }

  parsed_df$date_start <- as.Date(parsed_df$date_start)
  parsed_df$date_end <- as.Date(parsed_df$date_end)

  parsed_df$disposition <- c(
    R = "Returned to River",
    U = "Unknown",
    B = "Bypass",
    S = "Sampled",
    T = "Transported",
    H = "Held",
    X = "Unknown"
  )[parsed_df$releasecode1]

  parsed_df$censored <- c(
    R = FALSE,
    U = TRUE,
    B = TRUE,
    S = TRUE,
    T = FALSE,
    H = TRUE,
    X = TRUE
  )[parsed_df$releasecode1]

  parsed_df <- parsed_df[, names(parsed_df) != "releasecode1"]

  parsed_df$lifestage <- parsed_df$releasecode2
  parsed_df <- parsed_df[, names(parsed_df) != "releasecode2"]


  parsed_df$array_order <- parsed_df$releasecode3
  parsed_df <- parsed_df[, names(parsed_df) != "releasecode3"]


  # Need to remove the records where exceptions exist.

  # site_config <- DART_site_config() %>%
  #   rename(obs_site = sitecode, antenna_id = arraycodes) %>%
  #   separate_rows(antenna_id, sep = "\\s+") %>%
  #   arrange(desc(exception)) %>%
  #   group_by(across(obs_site:date_end)) %>%
  #   slice(1) %>%
  #   ungroup()

  return(parsed_df)
}

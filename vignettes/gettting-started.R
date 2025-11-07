## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  eval = FALSE,
  collapse = TRUE,
  comment = "#>",
  fig.width = 7, fig.height = 4,
  message = TRUE, warning = TRUE
)

## -----------------------------------------------------------------------------
#  # install.packages("remotes")
#  remotes::install_github("ryankinzer/PITmodelR")

## -----------------------------------------------------------------------------
#  library(PITmodelR)

## -----------------------------------------------------------------------------
#  # save the api key in the session environment
#  Sys.setenv(PTAGIS_API_KEY = "YOUR-PTAGIS-API-KEY")

## -----------------------------------------------------------------------------
#  codes <- get_project_codes()

## ----eval=FALSE---------------------------------------------------------------
#  yrs <- get_project_years(code = "CDR")

## ----eval=FALSE---------------------------------------------------------------
#  files <- get_mrr_files(code = "CDR", year = 2024)

## ----eval=FALSE---------------------------------------------------------------
#  mrr_file <- get_file_data(files$name[1])
#  mrr_file$session
#  mrr_file$events
#  mrr_file$session_pdv_fields
#  mrr_file$detail_pdv_fields
#  
#  df <- flatten_mrr_file(mrr_file)

## ----eval=FALSE, message=FALSE------------------------------------------------
#  filenames <- files$name[grepl('SCT', files$name)]
#  
#  # Batch workflow:
#  all_data <- get_batch_file_data(
#    filenames,
#    check_labels = "warn",         # checks if user defined fields have the same labels, can use "error" to stop on mismatches
#    keep_code_cols = FALSE,         # should we keep PDV/SPDV code columns and the user defined labels
#    label_conflict = "suffix",     # behavior for file label column collisions
#    use_codes_on_conflict = TRUE   # prefer consistent code columns across files
#  )
#  
#  # list of all individual files
#  names(all_data$files)
#  
#  # look at data for a single file
#  all_data$files$`CDR-2024-101-JCT.xml`
#  
#  # combined session and event data into a single dataset
#  df <- all_data$combined
#  
#  # label mismatch issues
#  all_data$issues

## ----eval=FALSE---------------------------------------------------------------
#  library(tidyverse)
#  
#  mark_group <- df %>%
#    filter(species_run_rear_type == '12W',
#           migration_year == 2025,
#           between(release_date, ymd(20240901), ymd(20241231)),
#           release_site == 'SECTRP',
#           pittag != "..........",
#           !grepl('RE', text_comments),
#           !grepl('Y', conditional_comments)
#    )

## ----eval=FALSE---------------------------------------------------------------
#  pittag <- mark_group$pittag[1]
#  tag_history <- get_tag_history(tag_code = pittag)

## ----eval=FALSE---------------------------------------------------------------
#  pittags <- mark_group$pittag
#  tag_history <- get_batch_tag_histories(tag_codes = pittags)

## ----eval=FALSE---------------------------------------------------------------
#  table(tag_history$event_type)
#  table(tag_history$site_code)

## ----eval=FALSE---------------------------------------------------------------
#  
#  locs <- c("SECTRP","ZEN","SFG","LGR","Down")
#  
#  locs <- list(
#    SECTRP = "SECTRP",
#    ZEN    = "ZEN",
#    SFG    = "SFG",
#    LGR    = c("GRJ","GRS"),
#    Down   = c("LMN","MCN","BON", "B2J", "BCC", "GOJ", "ICH", "JDJ", "LMJ", "MCJ", "PD7", "PD8", "PDW", "TWX")
#  )
#  
#  res <- build_mark_histories(
#    tag_history  = tag_history,
#    locs_def     = locs,
#    site_col     = "site_code",
#    tag_col      = "tag_code",
#    time_col     = "event_time",  # optional but helps order ties/revisits
#    enforce_order = TRUE,
#    keep_unknown  = FALSE
#  )
#  
#  ch_data <- res$ch_data      # tag_code, ch
#  ch_freq <- res$ch_freq      # ch, freq
#  res$dropped_summary         # any sites removed because not in `locs`
#  res$mapping                 # site -> occasion index


## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7, fig.height = 4,
  message = TRUE, warning = TRUE
)

## ----eval=FALSE---------------------------------------------------------------
#  # Install PITmodelR (development)
#  # install.packages("remotes")
#  remotes::install_github("yourusername/PITmodelR")
#  
#  # Optional: modeling packages you may use internally or downstream
#  install.packages(c("survival", "mgcv", "ggplot2", "dplyr", "tidyr"))

## -----------------------------------------------------------------------------
library(PITmodelR)

## ----eval=FALSE---------------------------------------------------------------
#  Sys.setenv(PTAGIS_API_KEY = "YOUR-PTAGIS-API-KEY")

## ----eval=FALSE---------------------------------------------------------------
#  # Example parameters
#  site   <- "LGR"
#  year   <- 2023
#  
#  # Pull site observations (auto-paginates by default)
#  obs <- get_site_observations(site_code = site, year = year)
#  dplyr::glimpse(obs)

## ----eval=FALSE---------------------------------------------------------------
#  tags <- c("384.1B79726A98", "384.1A2B3C4D5E", "3D9.1F00ABCD123")
#  hist_list <- lapply(tags, function(tc) get_tag_history(tag_code = tc))
#  hist <- dplyr::bind_rows(hist_list, .id = "tag_index")
#  dplyr::glimpse(hist)

## ----eval=FALSE---------------------------------------------------------------
#  # Hypothetical helper: convert events to detection histories by route/reach
#  # (Replace with your package's real helper names when implemented)
#  histories <- build_detection_histories(
#    observations = obs,
#    route_sites  = c("LGR","LGS","LMN","MCN"),  # example route
#    tag_col      = "tag_code",
#    time_col     = "event_time",
#    site_col     = "site_code"
#  )
#  
#  # Inspect the detection matrix / long-form representation
#  head(histories$long)
#  head(histories$wide)

## ----eval=FALSE---------------------------------------------------------------
#  # Add covariates for modeling (examples)
#  histories$long <- histories$long |>
#    dplyr::mutate(
#      doy = as.integer(strftime(event_time, format = "%j")),
#      hour = as.integer(strftime(event_time, format = "%H"))
#    )

## ----eval=FALSE---------------------------------------------------------------
#  # Minimal CJS-style survival by reach
#  cjs_fit <- fit_survival(
#    histories = histories,
#    model    = "cjs",                # or "multistate" if supported later
#    formula  = ~ 1                   # constant survival/detection
#  )
#  
#  summary(cjs_fit)
#  plot(cjs_fit)  # if you provide an S3 plot() method

## ----eval=FALSE---------------------------------------------------------------
#  cjs_fit_cov <- fit_survival(
#    histories = histories,
#    model     = "cjs",
#    formula   = list(
#      phi = ~ doy,   # survival ~ day of year
#      p   = ~ 1      # constant detection probability
#    )
#  )
#  
#  summary(cjs_fit_cov)

## ----eval=FALSE---------------------------------------------------------------
#  surv_tbl <- summarize_survival(cjs_fit_cov, level = 0.95)
#  print(surv_tbl)
#  
#  # Optional: export
#  # write.csv(surv_tbl, "reach_survival.csv", row.names = FALSE)

## ----eval=FALSE---------------------------------------------------------------
#  # Estimate timing with quantiles (e.g., 10%, 50%, 90% passage)
#  timing <- fit_timing(
#    observations = obs,
#    time_col     = "event_time",
#    by          = "day",        # aggregate by day
#    quantiles   = c(0.1, 0.5, 0.9)
#  )
#  
#  timing

## ----eval=FALSE---------------------------------------------------------------
#  timing_gam <- fit_timing(
#    observations = obs,
#    method       = "gam",
#    time_col     = "event_time",
#    by           = "day"
#  )
#  
#  summary(timing_gam$model)  # if you return the fitted mgcv object

## ----eval=FALSE---------------------------------------------------------------
#  # Simple passage curve
#  pass <- passage_curve(timing)
#  plot_timing(pass)
#  
#  # If your plot functions support additional arguments:
#  plot_timing(pass, add_points = TRUE, ci = TRUE)

## ----eval=FALSE---------------------------------------------------------------
#  # Survival by reach with CIs
#  plot_survival(surv_tbl)  # e.g., bar/point-range by reach
#  
#  # Arrival / cumulative passage
#  plot_timing(timing)
#  
#  # Save figures
#  # ggplot2::ggsave("survival_by_reach.png", width = 7, height = 4, dpi = 300)
#  # ggplot2::ggsave("cumulative_passage.png", width = 7, height = 4, dpi = 300)

## ----eval=FALSE---------------------------------------------------------------
#  # Save key artifacts
#  saveRDS(cjs_fit_cov, file = "cjs_fit_cov.rds")
#  saveRDS(timing,     file = "timing_quantiles.rds")
#  
#  # Export tidy tables
#  # readr::write_csv(surv_tbl, "reach_survival.csv")
#  # readr::write_csv(timing,   "timing_quantiles.csv")

## ----eval=FALSE---------------------------------------------------------------
#  site   <- "LGR"
#  year   <- 2023
#  
#  obs <- get_site_observations(site_code = site, year = year)
#  
#  histories <- build_detection_histories(
#    observations = obs,
#    route_sites  = c("LGR","LGS","LMN","MCN"),
#    tag_col      = "tag_code",
#    time_col     = "event_time",
#    site_col     = "site_code"
#  )
#  
#  cjs_fit <- fit_survival(
#    histories = histories,
#    model     = "cjs",
#    formula   = list(phi = ~ doy, p = ~ 1)
#  )
#  
#  surv_tbl <- summarize_survival(cjs_fit, level = 0.95)
#  
#  timing <- fit_timing(
#    observations = obs,
#    time_col     = "event_time",
#    by           = "day",
#    quantiles    = c(0.1, 0.5, 0.9)
#  )
#  
#  pass <- passage_curve(timing)
#  
#  # Visuals
#  plot_survival(surv_tbl)
#  plot_timing(pass)

## -----------------------------------------------------------------------------
sessionInfo()


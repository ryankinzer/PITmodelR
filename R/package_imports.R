# package-level imports
# these generate the needed NAMESPACE entries automatically

#' @import dplyr tidyr tibble vctrs httr xml2 jsonlite
#' @import stats utils graphics methods
NULL

# avoid R CMD check NOTE about non-standard evaluation / dplyr `.data`
utils::globalVariables(c(
  ".data",                              # used widely in dplyr pipelines
  "prev_max",                           # used in build_mark_histories()
  "detected", "ch", "freq", "interval",
  "leg", "travel_days", "estimate",
  "occ_idx"
))

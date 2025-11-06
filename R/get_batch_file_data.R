#' @title Download, Check, Flatten, and Combine MRR Files
#'
#' @description
#' A one-call convenience wrapper to download multiple PTAGIS mark-recapture-recovery (MRR) XML files,
#' check for PDV/SPDV label consistency across files, and return everything as a single list.
#'
#' @param filenames A character vector of MRR file names to download and process.
#' @param check_labels Character; one of \code{"warn"}, \code{"error"}, or \code{"ignore"}.
#'   Determines how to handle PDV/SPDV label inconsistencies across files.
#' @param keep_code_cols Logical, default \code{TRUE}. If \code{TRUE}, retains code columns (pdv*, spdv*) in flattened tibbles.
#' @param label_conflict Character; one of \code{"suffix"}, \code{"overwrite"}, or \code{"skip"}.
#'   Determines how to handle column name conflicts arising from labels when flattening MRR files.
#' @param use_codes_on_conflict Logical, default \code{TRUE}. If \code{TRUE}, code columns are preferred over label-derived columns
#'   when combining flattened tibbles to ensure a consistent schema.
#'
#' @return A list with three elements:
#' \itemize{
#'   \item \code{files} – named list of flattened tibbles, one per file
#'   \item \code{combined} – a single tibble combining "events" from all files
#'   \item \code{issues} – a data.frame of PDV/SPDV label inconsistencies detected across files
#' }
#'
#' @author Ryan Kinzer
#'
#' @export

get_batch_file_data <- function(filenames,
                                check_labels = c("warn","error","ignore"),
                                keep_code_cols = TRUE,
                                label_conflict = c("suffix","overwrite","skip"),
                                use_codes_on_conflict = TRUE) {

  check_labels   <- match.arg(check_labels)

  label_conflict <- match.arg(label_conflict)

  mrr_list <- download_mrr_files(filenames)

  issues <- check_pdv_label_consistency(mrr_list,
                                        which = "both",
                                        action = check_labels)

  files <- flatten_mrr_list(mrr_list,
                            keep_code_cols = keep_code_cols,
                            label_conflict = label_conflict)

  combined <- combine_flattened_mrr(files,
                                    use_codes_on_conflict = use_codes_on_conflict)

  list(files = files,
       combined = combined,
       issues = issues)

}

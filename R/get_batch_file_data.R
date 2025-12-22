#' @title Download, Check, Flatten, and Combine MRR Files
#'
#' @description
#' A one-call convenience wrapper to download multiple PTAGIS mark-recapture-recovery (MRR) XML files,
#' check for PDV/SPDV label consistency across files, and return everything as a single list.
#'
#' @param filenames A character vector of MRR file names to download and process.
#' @param check_labels Character; one of \code{"warn"}, \code{"error"}, or \code{"ignore"}.
#'   Determines how to handle PDV/SPDV label inconsistencies across files.
#' @param keep_code_cols Logical, default \code{TRUE}. If \code{TRUE}, retains code
#' columns (pdv*, spdv*) in flattened tibbles. If \code{FALSE}, replaces with user field names.
#' @param use_codes_on_conflict Logical, default \code{TRUE}. If \code{TRUE}, code columns are preferred over label-derived columns
#'   when combining flattened tibbles to ensure a consistent schema.
#'
#' @inheritParams flatten_mrr_file
#'
#' @return A list with three elements:
#' \itemize{
#'   \item \code{sessions} – named list of flattened tibbles, one per file
#'   \item \code{events} – a single tibble combining "events" from all files
#'   \item \code{issues} – a data.frame of PDV/SPDV label inconsistencies detected across files
#' }
#'
#' @author Ryan Kinzer
#'
#' @export

get_batch_file_data <- function(
    filenames,
    drop_pdvs = FALSE,
    attach_pdvs = !drop_pdvs,
    keep_mapping = attach_pdvs
) {

  mrr_list <- download_mrr_files(filenames, drop_pdvs = drop_pdvs)

  collapse_mrr_list(
    mrr_list,
    attach_pdvs = attach_pdvs,
    keep_mapping = keep_mapping
  )

}

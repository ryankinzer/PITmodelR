#' @title Flatten Set of Parsed MRR to Tibble
#'
#' @description
#' Flatten each parsed PTAGIS MRR XML file (output from \code{get_file_data(..., return="list")})
#' into a tibble. Returns a named list of tibbles, one per MRR file.
#'
#' @inheritParams flatten_mrr_file
#' @inheritParams check_pdv_label_consistency
#'
#' @return A named list of tibbles, one per MRR file.
#'
#' @author Ryan Kinzer
#'
#' @export

flatten_mrr_list <- function(mrr_list,
                             keep_code_cols = TRUE,
                             label_conflict = c("suffix","overwrite","skip")) {

  label_conflict <- match.arg(label_conflict)

  flats <- stats::setNames(vector("list", length(mrr_list)), names(mrr_list))

  for (i in seq_along(mrr_list)) {
    flats[[i]] <- flatten_mrr_file(mrr_list[[i]],
                                   keep_code_cols = keep_code_cols,
                                   label_conflict = label_conflict)
  }

  flats

}

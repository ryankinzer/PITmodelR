#' @title Flatten Set of Parsed MRR to Tibble
#'
#' @description
#' Flatten each parsed PTAGIS MRR XML file (output from \code{get_file_data(..., return="list")})
#' into a tibble. Returns a named list of tibbles, one per MRR file.
#'
#' @param mrr_list A list of parsed MRR objects, typically from \code{get_file_data(..., return="list")}.
#' @param keep_code_cols Logical, default \code{TRUE}. If \code{TRUE}, retains code columns (pdv*, spdv*)
#'   in the output tibbles.
#' @param label_conflict Character; one of \code{"suffix"}, \code{"overwrite"}, or \code{"skip"}.
#'   Determines how to handle column name conflicts arising from labels.
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

  flats <- setNames(vector("list", length(mrr_list)), names(mrr_list))

  for (i in seq_along(mrr_list)) {
    flats[[i]] <- flatten_mrr_file(mrr_list[[i]],
                                   keep_code_cols = keep_code_cols,
                                   label_conflict = label_conflict)
  }

  flats

}

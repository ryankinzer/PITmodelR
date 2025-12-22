#' @title Flatten Parsed MRR Files into Session and Event Tables
#'
#' @description
#' Converts a list of parsed MRR file objects into per-file flattened
#' session and event tables. Optionally attaches PDV/SPDV values as
#' character columns.
#'
#' @param mrr_list Named list of parsed MRR objects returned by
#'   \code{download_mrr_files()}.
#' @param attach_pdvs Logical; if TRUE, attaches PDV/SPDV values to
#'   session and event tables. Default FALSE.
#'
#' @return A named list where each element contains:
#'   \itemize{
#'     \item session – flattened session tibble
#'     \item events  – flattened events tibble
#'   }
#'
#' @author Mike Ackerman and Ryan Kinzer
#'
#' @export

flatten_mrr_list <- function(mrr_list,
                             keep_code_cols = TRUE,
                             label_conflict = c("suffix", "overwrite", "skip")) {

  label_conflict <- match.arg(label_conflict)

  flats <- stats::setNames(vector("list", length(mrr_list)), names(mrr_list))

  for (i in seq_along(mrr_list)) {
    flats[[i]] <- flatten_mrr_file(mrr_list[[i]],
                                   keep_code_cols = keep_code_cols,
                                   label_conflict = label_conflict)
  }

  flats

}

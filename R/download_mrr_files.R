#' @title Download and Parse Multiple MRR XML Files
#'
#' @description
#' Downloads and parses multiple mark-recapture-recovery (MRR) XML files.
#' Each file is read using \code{get_file_data()} and returned as a list.
#'
#' @inheritParams get_batch_file_data
#'
#' @return A named list where each element corresponds to one input file.
#'   Each element contains the parsed output from \code{get_file_data(..., return = "list")}.
#'
#' @author Ryan Kinzer
#'
#' @export

download_mrr_files <- function(filenames,
                               drop_pdvs = FALSE) {

  if (!is.character(filenames) || !length(filenames)) {
    stop("`filenames` must be a non-empty character vector.", call. = FALSE)
  }

  out <- stats::setNames(vector("list", length(filenames)), filenames)

  for (i in seq_along(filenames)) {
    fn <- filenames[i]
    out[[i]] <- get_file_data(fn, drop_pdvs = drop_pdvs)
  }

  out
}

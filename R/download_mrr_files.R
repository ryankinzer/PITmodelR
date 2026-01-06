#' @title Download and Parse Multiple PTAGIS MRR Files
#'
#' @description
#' Downloads and parses multiple PTAGIS mark–recapture–recovery (MRR) files
#' (JSON, XML, or legacy TXT/ASCII). Each file is processed independently via
#' \code{\link{get_file_data}} and returned as a named list.
#'
#' @param filenames Character vector of MRR file names to download and process.
#'   File names are used as names in the returned list.
#' @inheritParams get_file_data
#'
#' @return A named list where each element corresponds to one input file.
#'   Each element is the parsed output from \code{\link{get_file_data}} and
#'   contains at least \code{session} and \code{events}. If PDVs are retained
#'   upstream (i.e., \code{drop_pdvs = FALSE}), elements also contain
#'   \code{session_pdv_fields}, \code{detail_pdv_fields}, and \code{pdv_values}.
#'
#' @author Ryan Kinzer & Mike Ackerman
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

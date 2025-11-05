#' @title Download & Parse Multiple MRR .xml Files
#'
#' @param filenames
#'
#' @return a named list; each element is the parse list from `get_file_data(..., return = "list`
#'
#' @author Ryan Kinzer
#'
#' @export

download_mrr_files <- function(filenames) {

  if (!is.character(filenames) || !length(filenames)) {
    stop("`filenames` must be a non-empty character vector.", call. = FALSE)
  }

  out <- setNames(vector("list", length(filenames)), filenames)

  for (i in seq_along(filenames)) {
    fn <- filenames[i]
    out[[i]] <- get_file_data(fn, return = "list")
  }

  out

}

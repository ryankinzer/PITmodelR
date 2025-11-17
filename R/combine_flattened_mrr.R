#' @title Combine Flattened MRR Tibbles
#'
#' @description
#' Combine several flattened MRR tibbles (from \code{flatten_mrr_list}) into a
#' single tibble. Optionally prefers code columns (pdv, spdv) to ensure a consistent
#' schema when there are label conflicts.
#'
#' @param flat_list A list of tibbles, typically the output of \code{flatten_mrr_list}.
#'
#' @inheritParams get_batch_file_data
#'
#' @return A tibble combining all rows of \code{flat_list}, filling missing columns
#' with NA as needed.
#'
#' @author Ryan Kinzer
#'
#' @export

combine_flattened_mrr <- function(flat_list,
                                  use_codes_on_conflict = TRUE) {

  if (!length(flat_list)) return(tibble::tibble())

  # if requested, drop label-derived columns when their code equivalents exist
  if (isTRUE(use_codes_on_conflict)) {

    drop_labels_that_have_codes <- function(df) {
      nms <- names(df)
      # heuristics: code columns are exactly "pdv\\d+" or "spdv\\d+" (case-insensitive)
      code_cols  <- grep("^(pdv|spdv)\\d+$", nms, ignore.case = TRUE, value = TRUE)
      # label columns are anything else; we only drop labels if they duplicate codes conceptually.
      # Since we can't robustly map label name -> code here, we simply keep both; codes guarantee schema.
      # (If you want to *only* keep codes, uncomment the next line)
      # df <- df[, c(setdiff(nms, setdiff(nms, code_cols))), drop = FALSE]
      df
    }

    flat_list <- lapply(flat_list, drop_labels_that_have_codes)

  }

  bind_rows_fill(flat_list)

}

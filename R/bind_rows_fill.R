#' @title Bind Rows and Fill
#'
#' @description Bind rows while filling missing columns; adds .id column name
#' supplied
#'
#' @param lst A list of data frames to bind
#' @param id_name Name of the identifier column to add to the output tibble.
#'
#' @return A tibble with missing columns filled and an .id column added.
#'
#' @keywords internal

bind_rows_fill <- function(lst, id_name = "file") {

  if (!length(lst)) return(tibble::tibble())

  # union of columns
  cols <- Reduce(union, lapply(lst, names))

  # fill and add id
  filled <- vector("list", length(lst))

  nm <- names(lst)

  for (i in seq_along(lst)) {
    df <- lst[[i]]
    miss <- setdiff(cols, names(df))
    for (m in miss) df[[m]] <- NA
    df <- df[cols]
    df[[id_name]] <- if (length(nm)) nm[i] else as.character(i)
    filled[[i]] <- df
  }

  tibble::as_tibble(do.call(rbind, filled))

}

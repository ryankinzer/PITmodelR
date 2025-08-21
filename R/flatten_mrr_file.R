#' Flatten a parsed MRR list into one tibble
#'
#' Takes the output of get_file_data(..., return = "list") and:
#' 1) replicates session columns onto every events row, and
#' 2) replaces SPDV/PDV code columns with their labeled columns using the
#'    session/detail PDV mapping tables.
#'
#' @param mrr A list from get_file_data(..., return = "list")
#' @param keep_code_cols logical; keep original SPDV*/PDV* code columns? (default FALSE)
#' @param label_conflict What to do if a label name already exists:
#'   "suffix" (default) -> append "_label" to the new name,
#'   "overwrite" -> overwrite the existing column,
#'   "skip" -> don’t create the labeled column if there’s a conflict.
#' @return A tibble with session+event fields in wide form.
#' @export
flatten_mrr_file <- function(mrr,
                        keep_code_cols = FALSE,
                        label_conflict = c("suffix", "overwrite", "skip")) {
  stopifnot(is.list(mrr))
  label_conflict <- match.arg(label_conflict)

  # safe snake case
  to_snake <- function(x) {
    x <- gsub("\\s+", "_", trimws(x))
    x <- gsub("[^A-Za-z0-9_]", "_", x)
    x <- gsub("_+", "_", x)
    x <- gsub("\\.$", "", x)
    x <- gsub("\\.$", "", x)
    x <- gsub("\\A_+|_+\\Z", "", x)
    x <- gsub("([a-z0-9])([A-Z])", "\\1_\\2", x)
    tolower(x)
  }

  # Move values from code columns to new label-named columns
  apply_code_map <- function(df, map_df, code_col = "pdv_column", label_col = "label",
                             code_prefix = c("pdv", "spdv"),
                             keep_codes = FALSE,
                             conflict = c("suffix","overwrite","skip")) {
    conflict <- match.arg(conflict)
    if (is.null(df) || !nrow(df)) return(df)
    if (is.null(map_df) || !nrow(map_df)) return(df)

    code_prefix <- match.arg(code_prefix)
    # normalize mapping frame
    md <- map_df
    # tolerate alt names
    if (!code_col %in% names(md)) {
      alt <- grep("pdv", names(md), ignore.case = TRUE, value = TRUE)
      if (length(alt)) code_col <- alt[1]
    }
    if (!label_col %in% names(md)) {
      alt <- grep("label", names(md), ignore.case = TRUE, value = TRUE)
      if (length(alt)) label_col <- alt[1]
    }
    if (!all(c(code_col, label_col) %in% names(md))) return(df)

    md[[code_col]]  <- tolower(trimws(md[[code_col]]))
    md[[label_col]] <- to_snake(md[[label_col]])

    # normalize df names (lower for matching code cols)
    nml <- tolower(names(df))

    for (i in seq_len(nrow(md))) {
      code <- tolower(md[[code_col]][i])   # e.g., "pdv1" / "spdv1"
      lab  <- md[[label_col]][i]           # clean label
      if (!nzchar(code) || !nzchar(lab)) next

      # find the actual column name in df that matches this code (case-insensitive)
      idx <- which(nml == code)
      if (!length(idx)) next
      old <- names(df)[idx[1]]

      new <- lab
      if (new %in% names(df)) {
        if (identical(conflict, "suffix")) {
          # avoid collision
          k <- 1
          cand <- paste0(new, "_label")
          while (cand %in% names(df)) {
            k <- k + 1
            cand <- paste0(new, "_label", k)
          }
          new <- cand
        } else if (identical(conflict, "skip")) {
          next
        } else if (identical(conflict, "overwrite")) {
          # proceed; will overwrite
        }
      }

      df[[new]] <- df[[old]]
      if (!keep_codes) {
        df[[old]] <- NULL
        nml <- tolower(names(df))
      }
    }
    df
  }

  # Get pieces (tibbles)
  session  <- mrr$session
  events   <- mrr$events
  spdv_map <- mrr$session_pdv_fields
  pdv_map  <- mrr$detail_pdv_fields

  # If no events, return empty result with best-guess columns
  if (is.null(events) || !nrow(events)) {
    warning("No MRREvent rows found; returning empty tibble.", call. = FALSE)
    return(tibble::tibble())
  }

  # 1) Apply PDV mappings to events (PDV# -> label columns)
  events2 <- apply_code_map(events, pdv_map,
                            code_col = "pdv_column",
                            label_col = "label",
                            code_prefix = "pdv",
                            keep_codes = keep_code_cols,
                            conflict = label_conflict)

  # 2) Apply SPDV mappings to session (SPDV# -> label columns)
  session2 <- session
  if (!is.null(session2) && nrow(session2) == 1L) {
    session2 <- apply_code_map(session2, spdv_map,
                               code_col = "pdv_column",
                               label_col = "label",
                               code_prefix = "spdv",
                               keep_codes = keep_code_cols,
                               conflict = label_conflict)
  }

  # 3) Replicate session row across events and bind
  # Ensure session has columns; if empty, just return events2
  if (!is.null(session2) && nrow(session2) == 1L && ncol(session2) > 0) {
    # Avoid name collisions by suffixing session columns that already exist in events
    ses <- session2[rep(1, nrow(events2)), , drop = FALSE]
    col_clash <- intersect(names(ses), names(events2))
    if (length(col_clash)) {
      # suffix collisions with "_session"
      rename_map <- setNames(paste0(col_clash, "_session"), col_clash)
      names(ses)[match(names(ses), names(rename_map), nomatch = 0)] <-
        rename_map[na.omit(match(names(ses), names(rename_map)))]
    }
    out <- tibble::as_tibble(cbind(ses, events2, stringsAsFactors = FALSE))
  } else {
    out <- tibble::as_tibble(events2)
  }

  out
}

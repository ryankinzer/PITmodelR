#' @title Flatten Parsed MRR File
#'
#' @description
#' Converts the parsed output of \code{get_file_data(..., return = "list")} into a single
#' tidy tibble by:
#' \enumerate{
#'   \item Replicating all session-level fields onto each event row.
#'   \item Converting PDV/SPDV code columns (e.g., \code{pdv1}, \code{spdv2}) into
#'         human-readable label columns using the session and detail PDV mapping tables.
#' }
#'
#' @param mrr A list returned by \code{get_file_data(..., return = "list")}, containing
#'   \code{session}, \code{events}, and PDV/SPDV mapping tables.
#' @param keep_code_cols Logical; if \code{TRUE}, retain the original PDV*/SPDV* code
#'   columns in addition to the new label-named columns. Default is \code{FALSE}.
#' @param label_conflict Character indicating how to handle name conflicts when creating
#'   label-named columns. Options:
#'   \describe{
#'     \item{\code{"suffix"}}{Append \code{"_label"}, \code{"_label2"}, etc. (default).}
#'     \item{\code{"overwrite"}}{Overwrite the existing column.}
#'     \item{\code{"skip"}}{Do not create the label column if the name already exists.}
#'   }
#'
#' @return A tibble containing event-level rows with all session fields and
#'   labeled PDV/SPDV fields merged in.
#'
#' @author Ryan Kinzer
#'
#' @export

flatten_mrr_file <- function(mrr,
                             keep_code_cols = FALSE,
                             label_conflict = c("suffix", "overwrite", "skip")) {

  stopifnot(is.list(mrr))
  label_conflict <- match.arg(label_conflict)

  # ---- safe snake case helper ----
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

  # ---- apply PDV/SPDV code - label mapping ----
  apply_code_map <- function(
    df,
    map_df,
    code_col = "pdv_column",
    label_col = "label",
    code_prefix = c("pdv", "spdv"),
    keep_codes = FALSE,
    conflict = c("suffix", "overwrite", "skip")
  ) {

    conflict <- match.arg(conflict)
    if (is.null(df) || !nrow(df)) return(df)
    if (is.null(map_df) || !nrow(map_df)) return(df)

    code_prefix <- match.arg(code_prefix)

    # normalize mapping frame
    md <- map_df

    # tolerate alternate names
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
      code <- tolower(md[[code_col]][i])  # e.g. "pdv1"
      lab  <- md[[label_col]][i]          # clean label

      if (!nzchar(code) || !nzchar(lab)) next

      # locate matching code column
      idx <- which(nml == code)
      if (!length(idx)) next

      old <- names(df)[idx[1]]
      new <- lab

      # resolve name collisions
      if (new %in% names(df)) {
        if (identical(conflict, "suffix")) {
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
          # proceed
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

  # ---- extract MRR pieces ----
  session  <- mrr$session
  events   <- mrr$events
  spdv_map <- mrr$session_pdv_fields
  pdv_map  <- mrr$detail_pdv_fields

  # ---- empty events? ----
  if (is.null(events) || !nrow(events)) {
    warning("No MRREvent rows found; returning empty tibble.", call. = FALSE)
    return(tibble::tibble())
  }

  # ---- 1) apply PDV mappings to events ----
  events2 <- apply_code_map(
    events, pdv_map,
    code_col = "pdv_column",
    label_col = "label",
    code_prefix = "pdv",
    keep_codes = keep_code_cols,
    conflict = label_conflict
  )

  # ---- 2) apply SPDV mappings to session ----
  session2 <- session
  if (!is.null(session2) && nrow(session2) == 1L) {
    session2 <- apply_code_map(
      session2, spdv_map,
      code_col = "pdv_column",
      label_col = "label",
      code_prefix = "spdv",
      keep_codes = keep_code_cols,
      conflict = label_conflict
    )
  }

  # ---- 3) replicate session row across events ----
  if (!is.null(session2) && nrow(session2) == 1L && ncol(session2) > 0) {

    ses <- session2[rep(1, nrow(events2)), , drop = FALSE]

    # avoid name collisions (suffix "_session")
    col_clash <- intersect(names(ses), names(events2))
    if (length(col_clash)) {
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

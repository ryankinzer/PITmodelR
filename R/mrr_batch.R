# -----------------------------
# Batch MRR helpers
# -----------------------------

# Download & parse a set of MRR XML files
# Returns a named list: each element is the parsed list from get_file_data(..., return="list")
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

# Inspect PDV/SPDV label consistency across files
# which: "pdv", "spdv", "both"
# action: "warn", "error", "ignore"
# Returns a data.frame of issues (zero rows if none)
#' @export
check_pdv_label_consistency <- function(mrr_list,
                                        which = c("both","pdv","spdv"),
                                        action = c("warn","error","ignore")) {
  which  <- match.arg(which)
  action <- match.arg(action)

  # Early empty return with consistent schema
  empty_issues <- function() {
    data.frame(scope = character(), code = character(),
               files = character(), labels = character(),
               stringsAsFactors = FALSE)
  }
  if (!length(mrr_list)) return(empty_issues())

  # Normalize a mapping frame to (scope, file, code, label)
  norm_map <- function(mrr, file, scope = c("detail","session")) {
    scope <- match.arg(scope)
    map <- if (scope == "detail") mrr$detail_pdv_fields else mrr$session_pdv_fields
    if (is.null(map) || !nrow(map)) {
      return(data.frame(scope = scope, file = character(), code = character(), label = character(),
                        stringsAsFactors = FALSE))
    }
    nm <- names(map)
    col_code  <- if ("pdv_column" %in% nm) "pdv_column" else if ("pdv_col" %in% nm) "pdv_col" else grep("pdv", nm, value = TRUE)[1]
    col_label <- if ("label" %in% nm) "label" else grep("label", nm, value = TRUE)[1]
    if (is.na(col_code) || is.na(col_label)) {
      return(data.frame(scope = scope, file = character(), code = character(), label = character(),
                        stringsAsFactors = FALSE))
    }
    code  <- tolower(trimws(map[[col_code]]))
    label <- trimws(map[[col_label]])
    data.frame(scope = scope, file = file, code = code, label = label, stringsAsFactors = FALSE)
  }

  rows <- list()
  for (nm in names(mrr_list)) {
    mrr <- mrr_list[[nm]]
    if (which %in% c("both","spdv")) rows[[length(rows)+1]] <- norm_map(mrr, nm, scope = "session")
    if (which %in% c("both","pdv"))  rows[[length(rows)+1]] <- norm_map(mrr, nm, scope = "detail")
  }
  if (!length(rows)) return(empty_issues())

  maps <- do.call(rbind, rows)
  if (is.null(maps) || !nrow(maps)) return(empty_issues())

  # For each (scope, code), check if multiple labels used across files
  split_key <- interaction(maps$scope, maps$code, drop = TRUE, lex.order = TRUE)
  issues_list <- lapply(split(maps, split_key), function(df) {
    labs <- unique(df$label[nzchar(df$label)])
    if (length(labs) > 1) {
      data.frame(scope = df$scope[1],
                 code  = df$code[1],
                 files = paste(unique(df$file), collapse = ", "),
                 labels= paste(labs, collapse = " | "),
                 stringsAsFactors = FALSE)
    } else {
      NULL
    }
  })

  issues_list <- issues_list[!vapply(issues_list, is.null, logical(1))]
  issues <- if (length(issues_list)) do.call(rbind, issues_list) else empty_issues()

  if (nrow(issues) > 0) {
    msg <- paste0(
      "PDV/SPDV label mismatches detected across files:\n",
      paste0(" - [", issues$scope, "] ", issues$code, " : ", issues$labels,
             " (files: ", issues$files, ")"),
      collapse = "\n"
    )
    if (action == "error") stop(msg, call. = FALSE)
    if (action == "warn")  warning(msg, call. = FALSE)
  }

  issues
}


# Flatten each parsed MRR to a tibble; returns named list of tibbles
# keep_code_cols defaults to TRUE for cross-file harmonization
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

# Bind rows while filling missing columns; adds .id column name supplied
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
  # rbind
  tibble::as_tibble(do.call(rbind, filled))
}

# Combine flattened MRR tibbles into one
# If use_codes_on_conflict = TRUE, we prefer the code columns (pdv*, spdv*) to ensure a consistent schema
#' @export
combine_flattened_mrr <- function(flat_list,
                                  use_codes_on_conflict = TRUE) {
  if (!length(flat_list)) return(tibble::tibble())
  # If requested, drop label-derived columns when their code equivalents exist
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

# One-call convenience: download, check, flatten, and combine
# Returns a list: per_file (named list of tibbles), combined (tibble), issues (data.frame)
#' @export
get_batch_file_data <- function(filenames,
                                         check_labels = c("warn","error","ignore"),
                                         keep_code_cols = TRUE,
                                         label_conflict = c("suffix","overwrite","skip"),
                                         use_codes_on_conflict = TRUE) {
  check_labels   <- match.arg(check_labels)
  label_conflict <- match.arg(label_conflict)

  mrr_list <- download_mrr_files(filenames)

  issues <- check_pdv_label_consistency(mrr_list, which = "both", action = check_labels)

  files <- flatten_mrr_list(mrr_list,
                               keep_code_cols = keep_code_cols,
                               label_conflict = label_conflict)

  combined <- combine_flattened_mrr(files,
                                    use_codes_on_conflict = use_codes_on_conflict)

  list(files = files, combined = combined, issues = issues)
}

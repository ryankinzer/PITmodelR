#' @title Inspect PDV/SPDV Labels
#'
#' @description
#' Inspect PDV (Detail Project Defined Fields) and SPDV (Session Project
#' Defined Fields) label consistency across a list of parsed MRR files.
#' Detects conflicting labels across files.
#'
#' @param mrr_list named list of parsed MRR objects (output
#'   from \code{get_file_data(..., return="list")}).
#' @param which Character. Which fields to check: "both" (default), "pdv" (detail
#'   fields), or "spdv" (session fields).
#' @param action Character. What to do when inconsistencies are found:
#'   "warn" (default), "error", or "ignore".
#'
#' @return a data.frame of issues (zero rows if none)
#'
#' @keywords internal

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
      # return(data.frame(scope = scope, file = character(), code = character(), label = character(),
      #                   stringsAsFactors = FALSE))
      return(data.frame(scope = scope, file = NA_character_, code = NA_character_, label = NA_character_,
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

#' @title Subset events and output files based on defined groups.
#'
#' @description
#' Splits a large event object into groups based on \code{event_type} and
#' user defined groups within the data. After splitting each group is saved to
#' individual \code{.rds} files, and the user has the option to save an
#' additional \code{.txt} file of PIT-tag codes for easy upload into PTAGIS
#' queries (e.g., complete tag histories, interogation details).
#'
#'
#' @param events A single data.frame (or Tibble) object.
#' @param group_cols Character vector of grouping variables as character strings
#' @param event_type_value Character; one of \code{"Mark"} (default), \code{"Recapture"}, \code{"Tally"},\code{"Recovery"},\code{"Passive Recapture"},
#' @param out_dir Character string of desired output directory
#' @param remove_blank_tags Logical; if \code{TRUE} events containing \code{NULL} PIT-tag codes (i.e., dot-outs) will be removed from the returned \code{.rds} file.
#' @param save_TagIds Logical; if \code{TRUE} additional \code{.txt} files will be created
#'   for each defined group containing only PIT-tag codes.
#' @param unique_tags Logical; if \code{TRUE} if you want duplicate PIT-tag codes be removed from the \code{.txt} list.

#'
#' @author Ryan N. Kinzer
#'
#' @export
#'
save_event_groups <- function(events,
                              group_cols,
                              event_type_value = c("Mark", "Recapture", "Tally", "Recovery", "Passive Recapture"),
                              out_dir = NULL,
                              remove_blank_tags = TRUE,
                              save_TagIds = TRUE,
                              unique_tags = TRUE) {
  if (is.null(events) || !length(events) > 1) {
    stop("`events` must be a non-empty object.", call. = FALSE)
  }

  event_type_value <- match.arg(event_type_value)
  stopifnot(!is.null(out_dir))

  file_path <- file.path(out_dir, event_type_value)
  # create output folder if needed

  if (save_TagIds) {
    dir.create(file.path(file_path, "TagIds"),
               recursive = TRUE,
               showWarnings = FALSE)
  } else {
    file.path(out_dir, event_type_value)
    dir.create(file_path, recursive = TRUE, showWarnings = FALSE)
  }

  # make grouping table
  group_df <- events %>%
    distinct(across(all_of(group_cols)))

  # loop over each group
  walk(split(group_df, seq_len(nrow(group_df))), function(x) {
    # start with base filter
    dat <- events %>%
      filter(event_type == event_type_value) %>%
      filter(!is.na(pit_tag))

    if (remove_blank_tags) {
      dat <- dat %>%
        filter(pit_tag != "")
    }

    # apply group-specific filters
    for (nm in group_cols) {
      dat <- dat %>%
        filter(.data[[nm]] == x[[nm]])
    }

    pit_tags <- dat %>%
      pull(pit_tag)

    if (unique_tags) {
      pit_tags <- unique(pit_tags)
    }

    # build filename from grouping values
    file_stub <- paste(x[group_cols], collapse = "_")

    if (save_TagIds) {
      tag_file_name <- paste0(file_stub, "_TagIds.txt")
      tag_file_path <- file.path(file_path, "TagIds", tag_file_name)
      write_lines(pit_tags, tag_file_path)
    }


    dat_file_path <- paste0(file_path, "/", file_stub, "_", event_type_value, ".rds")
    saveRDS(dat, dat_file_path)
  })

  invisible(group_df)
}

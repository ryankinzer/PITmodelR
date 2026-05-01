#' @title Simulate PIT-tag CJS Data with Removals and Added Releases
#'
#' @description
#' Simulates downstream detection histories for testing CJS, multistate CJS,
#' and M-array workflows. Fish can survive between sites, be detected at sites,
#' be removed after detection, and new marked fish can be added at downstream sites.
#'
#' @param n_init Number of fish released at the first site.
#' @param n_add Optional integer vector of new releases added at each site.
#'   Length must equal number of sites. Usually first element should be 0.
#' @param sites Character vector of ordered site names.
#' @param S Survival probabilities between sites. Length must be length(sites) - 1.
#' @param p Detection probabilities at sites. Length must equal length(sites).
#'   The first value is ignored if the first site is release-only.
#' @param remove_prob Probability of removal after detection at each site.
#'   Length must equal length(sites).
#' @param start_date Starting date-time for simulated observations.
#' @param seed Optional random seed.
#'
#' @return Data frame with tag-level event records.
#'
#' @author Ryan N. Kinzer
#'
#' @export
simulate_cjs_events <- function(n_init = 1000,
                                n_add = NULL,
                                sites = paste0("site_", 1:5),
                                S = rep(0.8, length(sites) - 1),
                                p = c(1, rep(0.5, length(sites) - 1)),
                                remove_prob = rep(0, length(sites)),
                                start_date = as.POSIXct("2025-04-01", tz = "UTC"),
                                seed = NULL) {

  if (!is.null(seed)) set.seed(seed)

  n_sites <- length(sites)

  if (length(S) != n_sites - 1) {
    stop("S must have length length(sites) - 1.", call. = FALSE)
  }

  if (length(p) != n_sites) {
    stop("p must have length length(sites).", call. = FALSE)
  }

  if (length(remove_prob) != n_sites) {
    stop("remove_prob must have length length(sites).", call. = FALSE)
  }

  if (is.null(n_add)) {
    n_add <- integer(n_sites)
  }

  if (length(n_add) != n_sites) {
    stop("n_add must have length equal to length(sites).", call. = FALSE)
  }

  n_add[1] <- n_init

  out <- list()
  tag_counter <- 1L

  for (release_site in seq_len(n_sites)) {

    n_release <- n_add[release_site]

    if (n_release == 0) next

    for (f in seq_len(n_release)) {

      tag <- sprintf("SIM%08d", tag_counter)
      tag_counter <- tag_counter + 1L

      alive <- TRUE
      available <- TRUE

      for (site_idx in release_site:n_sites) {

        if (!alive || !available) break

        # survival from previous site to current site
        if (site_idx > release_site) {
          alive <- stats::rbinom(1, 1, S[site_idx - 1]) == 1
          if (!alive) break
        }

        # first site of release is known marked/released
        if (site_idx == release_site) {
          detected <- TRUE
        } else {
          detected <- stats::rbinom(1, 1, p[site_idx]) == 1
        }

        if (detected) {

          removed <- stats::rbinom(1, 1, remove_prob[site_idx]) == 1

          out[[length(out) + 1L]] <- data.frame(
            tag_code = tag,
            obs_site = sites[site_idx],
            first_obs = start_date + as.difftime(site_idx - 1, units = "days"),
            last_obs = start_date + as.difftime(site_idx - 1, units = "days"),
            last_disposition = ifelse(removed, "Removed", "Returned to River"),
            last_censored = removed,
            release_site = sites[release_site],
            release_site_idx = release_site,
            stringsAsFactors = FALSE
          )

          if (removed) {
            available <- FALSE
          }
        }
      }
    }
  }

  if (!length(out)) {
    return(data.frame(
      tag_code = character(),
      obs_site = character(),
      first_obs = as.POSIXct(character()),
      last_obs = as.POSIXct(character()),
      last_disposition = character(),
      last_censored = logical(),
      release_site = character(),
      release_site_idx = integer()
    ))
  }

  do.call(rbind, out)
}

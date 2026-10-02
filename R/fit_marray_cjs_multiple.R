#' @title Fit a CJS Model Using an M-array
#'
#' @description
#' Fits a CJS-style likelihood from a Burnham-style M-array for multiple release groups using \code{RTMB}. The model returns survival
#' and detection probability estimates. Reach-specific survival and detection probabilities may be estimated jointly for release groups.
#' For each detection site, a single detection probability parameter may be estimated for all release groups or a separate detection parameter may be
#' estimated for each release group. Release groups and intervals that should share a single survival parameter may also be defined.
#'
#' @param m_array_list A tibble with a release_group column, containing the release group or location
#' and an m_array column, containing data from \code{build_marray()}.
#'
#' @param shared_p A vector containing the names of sites for which a shared detection probability should be estimated for all release groups.
#' For example, shared_p may be defined as shared_p <- c("site_1", "site_2"). If shared_p is defined as "All", then a single detection probability parameter is estimated
#' for all detection sites.
#'
#' @param shared_phi A list containing the release groups and intervals for which a single survival parameter should be estimated. For example, shared_phi should be defined
#' as shared_phi <- list(list(groups = c("release_group_1", "release_group_2"), interval = c(2, 3)), list(groups = c("release_group_3", release_group_4"), interval = 3)). If
#' shared_phi is defined as "None", then a unique survival parameter is estimated for each release group and interval.
#'
#' @return List with \code{phi}, \code{p}, \code{fit}, and \code{st_err}.
#'
#' @author Michelle A. Briggs
#'
#' @export

fit_marray_cjs_multiple <- function(marray_list, shared_p, shared_phi) {

  #compile the m-array data
  mar_list <- lapply(marray_list$m_array, function(df) {
    count_cols <- c(grep("^m_", names(df), value = TRUE), "never")
    as.matrix(df[, count_cols, drop = FALSE])
  })

  #number of occations by release group
  n_occ <- sapply(mar_list, ncol)
  max_n_occ <- max(n_occ)

  #define dimensions of the "largest" m-array (with most occasions)
  max_group <- which.max(n_occ)
  target_dim <- dim(mar_list[[max_group]])

  #function to add trailing 0s to smaller m-arrays
  pad_to_dim <- function(mat, target_dim) {
    padded <- matrix(0, nrow = target_dim[1], ncol = target_dim[2])
    # place original values into matching column names, matching rows (top-aligned)
    padded[1:nrow(mat), 1:ncol(mat)] <- mat
    padded
  }

  mar_list_padded <- lapply(mar_list, pad_to_dim,
                            target_dim = target_dim)

  #convert list to array
  mar_array <- simplify2array(mar_list_padded)

  #number of release groups
  n_groups = dim(mar_array)[3]

  #lists of sites and release groups
  site_lists <- lapply(marray_list$m_array, function(df) df$site[-1])
  release_group_list <- lapply(marray_list$m_array, function(df) df$site[1])

  #matrix number of detection sites x release groups
  max_len <- max(sapply(site_lists, length))

  #matrix with detection sites for each release group
  site_mat <- sapply(site_lists, function(x) {
    length(x) <- max_len
    x
  })

  colnames(site_mat) <- release_group_list

  #number of unique detection sites
  n_sites = length(unique(unlist(site_lists)))

  #Make the p index matrix
  #cols = release groups, rows = occasions
  #unique number for each p parameter that will be estimated
  #uses shared_p to fill in matrix
  if (all(shared_p == "All")) {
    p_index_mat <- matrix(as.numeric(as.factor(site_mat)), nrow = max(n_occ) - 2, byrow = F)
  } else {
    v <- as.vector(t(site_mat))
    is_na     <- is.na(v)
    is_shared <- !is_na & v %in% shared_p
    is_other  <- !is_na & !is_shared

    out <- rep(NA_integer_, length(v))
    out[is_other]  <- seq_len(sum(is_other))                          # each non-shared cell gets its own number
    out[is_shared] <- sum(is_other) + match(v[is_shared], shared_p)

    p_index_mat <- matrix(out, nrow = nrow(site_mat), ncol = ncol(site_mat),
                          byrow = TRUE, dimnames = dimnames(site_mat))
  }

  colnames(p_index_mat) <- unlist(release_group_list)

  # df to identify which parameter goes with which detection site and release group
  p_param_map <- as.data.frame(cbind(
    as.vector(site_mat)[!is.na(as.vector(site_mat))],
    as.vector(p_index_mat)[!is.na(as.vector(p_index_mat))],
    colnames(p_index_mat)[col(p_index_mat)[!is.na(p_index_mat)]]
  ))
  colnames(p_param_map) <- c("site", "p_idx", "release_group")

  if(all(shared_p == "All")) {
    p_param_map <- p_param_map %>%
      mutate(release_group = NA) %>%
      distinct()
  } else {
    p_param_map <- p_param_map %>%
      mutate(release_group = if_else(site %in% shared_p, NA, release_group)) %>%
      distinct()
  }

  #Make the phi index matrix
  #cols = release groups, rows = occasions
  #unique number for each phi parameter that will be estimated
  #uses shared_phi to fill in matrix
  if(all(shared_phi == "None")) {

    phi_index_mat <- matrix(NA_integer_, nrow = nrow(site_mat), ncol = ncol(site_mat))
    phi_index_mat[!is.na(site_mat)] <- matrix(seq_len(sum(!is.na(site_mat))))
    colnames(phi_index_mat) <- unlist(release_group_list)

  } else {

    phi_index_mat <- matrix(NA_integer_, nrow = nrow(site_mat), ncol = ncol(site_mat))
    colnames(phi_index_mat) <- unlist(release_group_list)
    phi_index_mat[!is.na(site_mat)] <- matrix(seq_len(sum(!is.na(site_mat))))

    #function to re-number phi_index for shared values
    share_params <- function(idx_mat, shared_phi) {
      m <- idx_mat

      # helper: merge all parameter values found in a block of cells into one
      merge_vals <- function(m, block) {
        vals <- unique(block[!is.na(block)])
        if (length(vals) > 1) m[!is.na(m) & m %in% vals] <- min(vals)
        m
      }

      for (r in shared_phi) {
        rows  <- r$interval
        groups <- r$groups
        for (i in rows) m <- merge_vals(m, m[i, groups])
      }

      # renumber non-NA cells 1..N with no gaps; NAs are left alone
      ok <- !is.na(m)
      m[ok] <- match(m[ok], unique(m[ok]))
      m
    }

    phi_index_mat <- share_params(phi_index_mat, shared_phi)

  }

  #df to identify which parameters correspond to release groups and intervals
  from_site <- lapply(marray_list$m_array, function(df) df$site[-length(df$site)])
  phi_param_map <- as.data.frame(cbind(unlist(from_site),
                                       as.vector(site_mat)[!is.na(as.vector(site_mat))],
                                       as.vector(phi_index_mat)[!is.na(as.vector(phi_index_mat))],
                                       colnames(phi_index_mat)[col(phi_index_mat)[!is.na(phi_index_mat)]]
  ))
  colnames(phi_param_map) <- c("from_site", "to_site", "phi_idx", "release_group")



  #compile data and parameters
  dat <- list(mar = mar_array,
              p_index = p_index_mat,
              phi_index = phi_index_mat,
              n_occ = n_occ, # a vector
              n_groups = n_groups, #number of release groups
              max_n_occ = max_n_occ,
              n_sites = n_sites)

  par <- list(logit_phi = matrix(0, nrow = max(phi_index_mat, na.rm = T)), #one phi for each unique value in the idx mat
              logit_p = matrix(0, nrow = max(p_index_mat, na.rm = T)), #one p for each unique value in the idx mat
              logit_lambda = structure(rep(0, n_groups))) #unique lambda for each group

  #function to fit the m-array CJS model
  m_array_multiple_fun <- function(par){
    getAll(dat, par)
    "[<-" <- ADoverload("[<-")

    lambda = plogis(logit_lambda)

    phi <- numeric(sum(n_occ) - n_groups*2) #n_occ - 2 for each group
    p <- numeric(n_sites)

    p <- plogis(logit_p)
    phi <- plogis(logit_phi)

    nll <- 0

    for (g in 1:n_groups) { #loop over release groups

      n_occ_g <- n_occ[g]

      q_g <- numeric(n_occ_g - 2)

      pi_g <- matrix(0, nrow = n_occ_g - 1, ncol = n_occ_g)


      for (t in 1:(n_occ_g - 1)) { #loop over release occasions

        if (t < (n_occ_g - 1)) {

          #define q (probably of non-detection)
          p_idx <- p_index[t, g]

          q_g[t] <- 1 - p[p_idx]
        }
      }

      for (t in 1:(n_occ_g - 1)) { #loop over release occasions

        if (t < (n_occ_g - 1)) {

          #index with p and phi parameters to use based on occasion and group
          phi_idx <- phi_index[t, g]

          p_idx <- p_index[t, g]

          #main diagonal: probability of survival and recapture
          pi_g[t,t] <- phi[phi_idx] * p[p_idx]

          #above the diagonal
          if (t < n_occ_g - 2) {
            for (j in (t+1):(n_occ_g - 2)) {
              pj <- p[p_index[j, g]]
              phij <- phi[phi_index[, g]]
              pi_g[t, j] = prod(phij[t:j]) * prod(q_g[t:(j-1)]) * pj
            }
          }

          pi_g[t, n_occ_g - 1] = prod(phij[t:(n_occ_g - 2)]) * prod(q_g[t:(n_occ_g - 2)]) * lambda[g]

        } else {

          pi_g[t, t] <- lambda[g]
        }
      }

      #last column: probability of non-recapture
      for (t in 1:(n_occ_g - 1)) {
        pi_g[t, n_occ_g] <- 1 - sum(pi_g[t, 1:(n_occ_g-1)])
      }

      #likelihood
      for (t in 1:(n_occ_g - 1)) {
        nll <- nll - dmultinom(mar[t, t:n_occ_g, g], prob = pi_g[t, t:n_occ_g], log = TRUE)
      }
    }

    REPORT(phi)
    REPORT(p)
    REPORT(lambda)
    REPORT(logit_phi)
    REPORT(logit_p)

    ADREPORT(phi)
    ADREPORT(p)
    ADREPORT(lambda)
    ADREPORT(logit_phi)
    ADREPORT(logit_p)

    nll

  }

  obj <- RTMB::MakeADFun(m_array_multiple_fun, par)
  obj$fn()
  obj$gr()

  opt <- nlminb(obj$par, obj$fn, obj$gr, obj$he,control = list(eval.max = 1e4, iter.max = 1e4))
  sdr <- RTMB::sdreport(obj)

  obj$report()
  rep <- obj$report()

  st_err <- as.list(sdr, "Std", report = T)

  fit <- list(rep, st_err)
  fit



  #format output

  #format p
  p <- as.data.frame(cbind(fit[[1]]$p, fit[[1]]$logit_p, fit[[2]]$logit_p))
  p <- p %>%
    rownames_to_column(var = "p_idx") %>%
    rename(p = V1, logit_p = V2, se_logit_p = V3) %>%
    dplyr::mutate(logit_lcl = logit_p - 1.96*se_logit_p,
                  logit_ucl = logit_p + 1.96*se_logit_p,
                  lcl = plogis(logit_lcl),
                  ucl = plogis(logit_ucl)) %>%
    dplyr::full_join(p_param_map, by = "p_idx") %>%
    dplyr::select(p, lcl, ucl, site, release_group)

  #format phi
  fit[[1]]$phi

  from_site <- lapply(marray_list$m_array, function(df) df$site[-length(df$site)])

  phi <- as.data.frame(cbind(fit[[1]]$phi, fit[[1]]$logit_phi, fit[[2]]$logit_phi)) %>%
    dplyr::rename(phi = V1,
                  logit_phi = V2,
                  se_logit_phi = V3) %>%
    dplyr::mutate(logit_lcl = logit_phi - 1.96*se_logit_phi,
                  logit_ucl = logit_phi + 1.96*se_logit_phi,
                  lcl = plogis(logit_lcl),
                  ucl = plogis(logit_ucl)) %>%
    rownames_to_column(var = "phi_idx") %>%
    dplyr::left_join(phi_param_map, by = "phi_idx") %>%
    dplyr::select(release_group, from_site, to_site, phi, lcl, ucl)

  #list of output
  marray_output <- list(
    p = p,
    phi = phi,
    fit = rep,
    st_err = st_err)
  marray_output

}

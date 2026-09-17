#' @title Fit a CJS Model Using an M-array
#'
#' @description
#' Fits a CJS-style likelihood from a Burnham-style M-array for multiple release groups using \code{RTMB}. The model returns survival
#' and detection probability estimates. Detection probability is estimated jointly for all release groups
#' and survival is estimated independently.
#'
#' @param m_array_list A tibble with a release_group column, containing the release group or location
#' and an m_array column, containing data from \code{build_marray()}.
#'
#' @return List with \code{phi}, \code{p}, \code{fit}, and \code{st_err}.
#'
#' @author Michelle A. Briggs
#'
#' @export

fit_marray_cjs_multiple <- function(marray_list) {

  #compile the m-array data
  mar_list <- lapply(marray_list$m_array, function(df) {
    count_cols <- c(grep("^m_", names(df), value = TRUE), "never")
    as.matrix(df[, count_cols, drop = FALSE])
  })

  n_occ <- sapply(mar_list, ncol)
  max_n_occ <- max(n_occ)

  max_group <- which.max(n_occ)
  target_dim <- dim(mar_list[[max_group]])

  pad_to_dim <- function(mat, target_dim) {
    padded <- matrix(0, nrow = target_dim[1], ncol = target_dim[2])
    # place original values into matching column names, matching rows (top-aligned)
    padded[1:nrow(mat), 1:ncol(mat)] <- mat
    padded
  }

  mar_list_padded <- lapply(mar_list, pad_to_dim,
                            target_dim = target_dim)

  mar_array <- simplify2array(mar_list_padded)

  n_groups = dim(mar_array)[3]

  #p index matrix
  site_lists <- lapply(marray_list$m_array, function(df) df$site[-1])
  max_len <- max(sapply(site_lists, length))

  site_mat <- sapply(site_lists, function(x) {
    length(x) <- max_len
    x
  })

  n_sites = length(unique(unlist(site_lists)))

  #each detection site is assigned a number
  #they are alphabetical
  p_index_mat <- matrix(as.numeric(as.factor(site_mat)), nrow = max(n_occ) - 2, byrow = F)

  #phi index matrix
  #using the entries in the p_index_mat as a template
  phi_index_mat <- matrix(NA_integer_, nrow = nrow(site_mat), ncol = ncol(site_mat))

  phi_index_mat[!is.na(site_mat)] <- matrix(seq_len(sum(!is.na(site_mat))))

  dimnames(phi_index_mat) <- dimnames(site_mat)

  #compile data and parameters
  dat <- list(mar = mar_array,
              p_index = p_index_mat,
              phi_index = phi_index_mat,
              n_occ = n_occ, # a vector
              n_groups = n_groups, #number of release groups
              max_n_occ = max_n_occ,
              n_sites = n_sites)

  par <- list(logit_phi = matrix(0, nrow = sum(n_occ) - n_groups*2),
              logit_p = matrix(0, nrow = n_sites), #one p for each detection site
              logit_lambda = structure(rep(0, n_groups))) #unique lambda for each group

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
  site_id <- as.data.frame(cbind(unique(unlist(site_lists)), as.factor(unique(unlist(site_lists)))))
  colnames(site_id) <- c("site", "num")

  p <- as.data.frame(cbind(fit[[1]]$p, fit[[1]]$logit_p, fit[[2]]$logit_p))
  p <- p %>%
    rownames_to_column(var = "num") %>%
    rename(p = V1, logit_p = V2, se_logit_p = V3) %>%
    dplyr::mutate(logit_lcl = logit_p - 1.96*se_logit_p,
           logit_ucl = logit_p + 1.96*se_logit_p,
           lcl = plogis(logit_lcl),
           ucl = plogis(logit_ucl)) %>%
    dplyr::full_join(site_id, by = "num") %>%
    dplyr::select(p, lcl, ucl, site)

  #format phi
  fit[[1]]$phi

  from_site <- lapply(marray_list$m_array, function(df) df$site[-length(df$site)])

  phi_id <- as.data.frame(site_mat)
  colnames(phi_id) <- marray_list$release_group

  phi_id <- phi_id %>%
    dplyr::pivot_longer(cols = 1:dim(phi_id)[2], names_to = "release_group", values_to = "to_site", cols_vary = "slowest") %>%
    dplyr::filter(!is.na(to_site))

  phi <- cbind(phi_id, fit[[1]]$phi, fit[[1]]$logit_phi, fit[[2]]$logit_phi) %>%
    dplyr::rename(phi = 'fit[[1]]$phi',
           logit_phi = 'fit[[1]]$logit_phi',
           se_logit_phi = 'fit[[2]]$logit_phi') %>%
    dplyr::mutate(logit_lcl = logit_phi - 1.96*se_logit_phi,
           logit_ucl = logit_phi + 1.96*se_logit_phi,
           lcl = plogis(logit_lcl),
           ucl = plogis(logit_ucl)) %>%
    dplyr::select(release_group, to_site, phi, lcl, ucl) %>%
    dplyr::mutate(from_site = unlist(from_site))

  marray_output <- list(
    p = p,
    phi = phi,
    fit = rep,
    st_err = st_err)
  marray_output

}

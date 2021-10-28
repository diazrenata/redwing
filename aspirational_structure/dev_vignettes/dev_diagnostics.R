library(dplyr)
library(rwar)
library(BBSsize)
library(brms)
library(tidybayes)
library(ggplot2)
theme_set(theme_bw())
library(drake)

## Set up the cache and config
db <- DBI::dbConnect(RSQLite::SQLite(), here::here("aspirational_structure", "drake_caches", "sim_means-cache.sqlite"))
cache <- storr::storr_dbi("datatable", "keystable", db)
cache$del(key = "lock", namespace = "session")
cached(cache=cache)

loadd(fits_ssims_actual_sims_bbs_rtrg_116_18, cache = cache)

some_fits <- fits_ssims_actual_sims_bbs_rtrg_116_18

extract_diagnostics <- function(some_fits) {

  te_diagnostics <- list()

  for(mod_name in names(some_fits$te_brms)) {
    nuts <- brms::nuts_params(some_fits$te_brms[[mod_name]])
    rhats <- brms::rhat(some_fits$te_brms[[mod_name]]) %>%
      t() %>%
      as.data.frame()
    colnames(rhats) <- paste0("rhat_", colnames(rhats))

    neffs <- brms::neff_ratio(some_fits$te_brms[[mod_name]]) %>%
      t() %>%
      as.data.frame()
    colnames(neffs) <- paste0("neff_", colnames(neffs))

    divergents <- nuts %>%
      dplyr::filter(Parameter == "divergent__") %>%
      dplyr::summarize(divergent_sum = sum(Value)) %>%
      dplyr::ungroup()

    te_diagnostics[[mod_name]] <- dplyr::bind_cols(neffs, rhats) %>%
      dplyr::mutate(divergent_sum = divergents$divergent_sum,
                    model = mod_name,
                    currency = "energy")
  }
  tb_diagnostics <- list()

  for(mod_name in names(some_fits$tb_brms)) {
    nuts <- brms::nuts_params(some_fits$tb_brms[[mod_name]])
    rhats <- brms::rhat(some_fits$tb_brms[[mod_name]]) %>%
      t() %>%
      as.data.frame()
    colnames(rhats) <- paste0("rhat_", colnames(rhats))

    neffs <- brms::neff_ratio(some_fits$tb_brms[[mod_name]]) %>%
      t() %>%
      as.data.frame()
    colnames(neffs) <- paste0("neff_", colnames(neffs))

    divergents <- nuts %>%
      dplyr::filter(Parameter == "divergent__") %>%
      dplyr::summarize(divergent_sum = sum(Value)) %>%
      dplyr::ungroup()

    tb_diagnostics[[mod_name]] <- dplyr::bind_cols(neffs, rhats) %>%
      dplyr::mutate(divergent_sum = divergents$divergent_sum,
                    model = mod_name,
                    currency = "biomass")
  }

  all_diagnostics <- dplyr::bind_rows(
    dplyr::bind_rows(te_diagnostics),
    dplyr::bind_rows(tb_diagnostics))

  return(all_diagnostics)
}

extract_diagnostics(some_fits)

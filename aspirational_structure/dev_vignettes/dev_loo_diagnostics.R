library(dplyr)
library(ggplot2)
library(brms)
library(tidybayes)
library(BBSsize)

load(here::here("aspirational_structure", "dev_vignettes", "3_80_for_diag.Rds"))


fit_stanlm <- function(some_sims) {

  # Fit a brm on total_energy
  te_brm_full <- rstanarm::stan_glm(total_energy ~ (timeperiod * source) , data = some_sims, iter =8000, thin = 4)
  te_brm_nosource <- rstanarm::stan_glm(total_energy ~ (timeperiod), data = some_sims, iter =8000, thin = 4)
  te_brm_notime <- rstanarm::stan_glm(total_energy ~ 1, data = some_sims, iter =8000, thin = 4)

  te_brms = list(
    te_brm_full = te_brm_full,
    te_brm_nosource = te_brm_nosource,
    te_brm_notime = te_brm_notime
  )


  # Fit the brm on total_biomass
  tb_brm_full <- rstanarm::stan_glm(total_biomass ~ (timeperiod * source) , data = some_sims, iter = 8000, thin = 4)
  tb_brm_nosource <- rstanarm::stan_glm(total_biomass ~ (timeperiod) , data = some_sims, iter = 8000, thin = 4)
  tb_brm_notime <- rstanarm::stan_glm(total_biomass ~ 1 , data = some_sims, iter = 8000, thin = 4)


  tb_brms = list(
    tb_brm_full = tb_brm_full,
    tb_brm_nosource = tb_brm_nosource,
    tb_brm_notime = tb_brm_notime
  )

  return(list(
    te_brms = te_brms,
    tb_brms = tb_brms,
    matssname =some_sims$matssname[1],
    simtype = some_sims$simtype[1]
  ))

}

compare_stanarms <- function(brms_fits) {

  brms_loos<- lapply(brms_fits, rstanarm::loo, k_threshold= .7)

  brms_comparison <- rstanarm::loo_compare(brms_loos) %>%
    as.data.frame() %>%
    dplyr::mutate(model = row.names(.),
                  rank = dplyr::row_number())

  return(brms_comparison)

}

compare_both_stanarms<- function(some_brms_fits) {

  biomass <- compare_stanarms(brms_fits = some_brms_fits$tb_brms)
  energy <- compare_stanarms(brms_fits = some_brms_fits$te_brms)

  both_comparisons <- dplyr::bind_rows(biomass = biomass, energy = energy, .id = "currency") %>%
    dplyr::mutate(matssname = some_brms_fits$matssname,
                  simtype = some_brms_fits$simtype[1])


  return(both_comparisons)

}


stanfits <- fit_stanlm(ssims_bbs_rtrg_3_80_actual)

stancomps <- compare_both_stanarms(stanfits)

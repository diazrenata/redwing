library(rwar)
library(BBSsize)
library(dplyr)
library(ggplot2)
slice_dat <- function(dataset, keep_years = c(1988:2018)) {

  keep_rows <- which(dataset$covariates$year %in% keep_years)

  dat_out <- dataset

  dat_out$abundance <- dat_out$abundance[keep_rows, ]
  dat_out$covariates <- dat_out$covariates[keep_rows,]

  dat_out
}

resp_dat <- function(dataset, resp_seed = 1989) {
  #
  #   spTotalAbunds <- colSums(dataset$abundance)
  #   spRelAbunds <- spTotalAbunds / sum(spTotalAbunds)

  spRelAbunds <- dataset$abundance/ rowSums(dataset$abundance)
  spRelAbunds <- colSums(spRelAbunds) / nrow(spRelAbunds)

  draw_year <- function(year_row, spRelAbunds) {
    as.data.frame(t(rmultinom(1, size  = sum(year_row), prob = spRelAbunds)))
  }

  set.seed(resp_seed)

  newAbund <- apply(dataset$abundance, MARGIN = 1, FUN = draw_year, spRelAbunds = spRelAbunds)
  set.seed(NULL)
  dataset$abundance <- bind_rows(newAbund)

  dataset
}

calc_sv <- function(isd) {

  sv_isd <- isd$isd %>%
    mutate(energy = estimate_b(mass)) %>%
    group_by(year) %>%
    summarize(abundance = dplyr::n(),
              total_biomass = sum(mass),
              total_energy = sum(energy)) %>%
    ungroup()%>%
    rename(timeperiod = year)

  bind_cols(sv_isd, isd$metadata$location)
}

whole_thing <- function(dat) {
  dat_sliced <-slice_dat(dat)

  dat_resp <- resp_dat(dat_sliced, resp_seed = 1989)

  dat_isd <- simulate_isd_ts(dat_sliced, isd_seed = 1989)

  dat_resp_isd <- simulate_isd_ts(dat_resp, isd_seed = 1989)


  real_sv <- calc_sv(dat_isd) %>%
    mutate(source = "real")
  sim_sv <- calc_sv(dat_resp_isd) %>%
    mutate(source = "sim")

  both_sv <- bind_rows(real_sv, sim_sv)  %>%
    mutate(fsource = as.factor(source),
           matssname = paste0("bbs_rtrg_", dat$metadata$location$route, "_", dat$metadata$location$statenum),
           simtype = "actual")

  both_sv
}

pick_gam <- function(sims) {

  linear_gam_full <- gam(total_biomass ~ timeperiod * source, data= sims, method = "REML", family = tw(), select= T)


  linear_gam_noint <- gam(total_biomass ~ timeperiod + source, data= sims, method = "REML", family = tw(), select= T)

  linear_gam_nos <- gam(total_biomass ~ timeperiod, data= sims, method = "REML", family = tw(), select= T)

  linear_gam_notime <- gam(total_biomass ~ 1, data = sims, method = "REML", family = tw(), select= T)

  linear_aics <- data.frame(
    model = c('full', 'no_interaction', 'no_source', 'no_time'),
    aic = c(AIC(linear_gam_full), AIC(linear_gam_noint), AIC(linear_gam_nos), AIC(linear_gam_notime)),
    type = "linear"
  )


  nonlinear_gam_full <- gam(total_biomass ~ fsource + s(timeperiod, by = fsource), data= sims, method = "REML", family = tw(), select= T)


  nonlinear_gam_noint <- gam(total_biomass ~ fsource + s(timeperiod), data= sims, method = "REML", family = tw(), select= T)

  nonlinear_gam_nos <- gam(total_biomass ~ s(timeperiod), data= sims, method = "REML", family = tw(), select= T)

  nonlinear_gam_notime <- gam(total_biomass ~ 1, data = sims, method = "REML", family = tw(), select= T)

  nonlinear_aics <- data.frame(
    model = c('full', 'no_interaction', 'no_source', 'no_time'),
    aic = c(AIC(nonlinear_gam_full), AIC(nonlinear_gam_noint), AIC(nonlinear_gam_nos), AIC(nonlinear_gam_notime)),
    type = "nonlinear"
  )

  all_aics <- bind_rows(linear_aics, nonlinear_aics) %>%
    mutate(matssname = sims$matssname[1])

  all_aics

}

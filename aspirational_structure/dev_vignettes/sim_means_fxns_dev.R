
make_nochange_sims <- function(dat) {

  dat_nochange <- dat

  begin_rows <- which(dat_nochange$covariates$year %in% c(1988:1992))
  end_rows <- which(dat_nochange$covariates$year %in% c(2014:2018))

  dat_nochange$abundance[end_rows, ] <- dat_nochange$abundance[begin_rows, ]

  dat_nochange_gmms <- rwar::construct_sampling_gmm(dat_nochange, n_isd_draws = 1)
  nochange_sims <- rwar::draw_communities_wrapper(dat_nochange, sampling_gmms = dat_nochange_gmms, ndraws = 10)
nochange_sims$simtype = "nochange"

  return(nochange_sims)

}

make_nosizechange_sims <- function(dat) {
  dat_gmms <- construct_sampling_gmm(dat, n_isd_draws = 1)

  nosizechange_gmms <- dat_gmms
  nosizechange_gmms$end <- nosizechange_gmms$begin %>% mutate(timeperiod = "end")

  nosizechange_sims <- draw_communities_wrapper(dat, sampling_gmms = nosizechange_gmms, ndraws = 10)

  nosizechange_sims$simtype = "nosizechange"
  return(nosizechange_sims)
}


make_actual_sims <- function(dat){
  dat_gmms <- construct_sampling_gmm(dat, n_isd_draws = 1)
  sims <- draw_communities_wrapper(dat, sampling_gmms = dat_gmms, ndraws = 10)
  sims$simtype = "actual"
  return(sims)
}

summarize_sims <- function(sims) {

  sims <- sims %>% filter(source != "raw") %>%
    group_by(timeperiod, source, year, matssname,simtype) %>%
    summarize(total_energy = mean(total_energy),
              total_biomass = mean(total_biomass),
              ndraws = dplyr::n()) %>%
    ungroup()

  return(sims)

}

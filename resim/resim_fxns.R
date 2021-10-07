just_isd <- function(ts_comp, isd_seed = NULL) {
  library(BBSsize)
  BBSsize::simulate_isd_ts(ts_comp, isd_seed = isd_seed)$isd
}

construct_sampling_gmm <- function(ts_comp, n_isd_draws = 25, initial_isd_seed = NULL, begin_years = 1988:1992, end_years = 2014:2018) {

  if(is.null(initial_isd_seed)) {
    set.seed(NULL)
    initial_isd_seed <- sample(1:1000000, 1)
  }

  isd_seeds <- sample.int(10000000, size = n_isd_draws)

  ts_isds_many <- lapply(isd_seeds, just_isd, ts_comp = ts_comp)
  names(ts_isds_many) <- 1:n_isd_draws
  ts_isds_many <- dplyr::bind_rows(ts_isds_many, .id = "sim")

  ts_isds_begin <- dplyr::filter(ts_isds_many, year %in% begin_years)
  ts_isds_end <- dplyr::filter(ts_isds_many, year %in% end_years)

  ts_gmm_begin <- rwar::add_gmm(ts_isds_begin)
  ts_gmm_end <- rwar::add_gmm(ts_isds_end)

  ts_gmm_begin <- dplyr::mutate(ts_gmm_begin, timeperiod = "begin")
  ts_gmm_end <- dplyr::mutate(ts_gmm_begin, timeperiod = "end")

  return(list(begin = ts_gmm_begin, end = ts_gmm_end))

}


draw_individuals <- function(nind, sampling_gmm, draw_seed = NULL) {


  if(is.null(draw_seed)) {
    set.seed(NULL)
    draw_seed <- sample(1:1000000, 1)
  }

  drawn_individuals <- data.frame(
    logmass = sample(sampling_gmm$mass, nind, T, sampling_gmm$density),
    isd_timeperiod = sampling_gmm$timeperiod[1],
    sampling_seed = draw_seed
  )

  drawn_individuals
}

add_drawn_individuals <- function(timeperiod_isd, sampling_gmm, draw_seed = NULL) {

  ntodraw = nrow(timeperiod_isd)

  drawn <- draw_individuals(ntodraw, sampling_gmm, draw_seed)

  timeperiod_isd <- timeperiod_isd %>%
    dplyr::select(-mass, -id) %>%
    dplyr::bind_cols(drawn) %>%
    dplyr::mutate(mass = exp(logmass)) %>%
    dplyr::mutate(energy = BBSsize::estimate_b(mass))

  timeperiod_isd
}

draw_communities <- function(ts_comp, begin_years = 1988:1992, end_years = 2014:2018, draw_seed = NULL, sampling_gmms = NULL, initial_isd_seed = NULL, raw_isd_seed = NULL) {

  if(is.null(sampling_gmms)) {

    sampling_gmms <- construct_sampling_gmm(ts_comp, begin_years = begin_years, end_years = end_years, initial_isd_seed = NULL)

  }

  if(is.null(raw_isd_seed)) {
    set.seed(NULL)
    raw_isd_seed <- sample(1:1000000, 1)
  }

  raw_isd <- BBSsize::simulate_isd_ts(ts_comp, isd_seed = raw_isd_seed)$isd

  begin_isd <- dplyr::filter(raw_isd, year %in% begin_years) %>%
    dplyr::mutate(timeperiod = "begin")
  end_isd <- dplyr::filter(raw_isd, year %in% end_years) %>%
    dplyr::mutate(timeperiod = "end")

  begin_individuals <- add_drawn_individuals(begin_isd, sampling_gmms$begin, draw_seed = draw_seed)

  end_individuals <- add_drawn_individuals(end_isd, sampling_gmms$end, draw_seed = draw_seed)

  end_individuals_sim <- add_drawn_individuals(end_isd, sampling_gmms$begin, draw_seed = draw_seed)


  all_individuals <- dplyr::bind_rows(begin_individuals, end_individuals, end_individuals_sim)

  all_svs <- all_individuals %>%
    dplyr::group_by(year, timeperiod, isd_timeperiod, sampling_seed, isd_seed) %>%
    dplyr::summarize(total_abundance = dplyr::n(),
                     total_mass = sum(mass),
                     total_energy = sum(energy)) %>%
    dplyr::ungroup() %>%
    dplyr::bind_cols(as.data.frame(ts_comp$metadata$location))

  all_svs
}

draw_communities_wrapper <- function(ts_comp, begin_years = 1988:1992, end_years = 2014:2018, ndraws = 100, draw_seed = NULL, sampling_gmms = NULL, initial_isd_seed = NULL, raw_isd_seed = NULL) {

  drawn_communities <- replicate(ndraws, draw_communities(ts_comp, begin_years = begin_years, end_years = end_years, draw_seed = draw_seed, sampling_gmms = sampling_gmms, initial_isd_seed = initial_isd_seed, raw_isd_seed = raw_isd_seed), simplify = F)

  names(drawn_communities) <- 1:ndraws

  dplyr::bind_rows(drawn_communities, .id = "sim_iteration")

}

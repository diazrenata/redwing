
#' Simulate dynamics repeating begin dynamics for end
#'
#' @param dat matss-style
#'
#' @return sims drawn as if begin years were also the end years
#' @export
make_nochange_sims <- function(dat) {

  dat_nochange <- dat

  begin_rows <- which(dat_nochange$covariates$year %in% c(1988:1992))
  end_rows <- which(dat_nochange$covariates$year %in% c(2014:2018))

  dat_nochange$abundance[end_rows, ] <- dat_nochange$abundance[begin_rows, ]

  dat_nochange_gmms <- rwar::construct_sampling_gmm(dat_nochange, n_isd_draws = 10)
  nochange_sims <- rwar::draw_communities_wrapper(dat_nochange, sampling_gmms = dat_nochange_gmms, ndraws = 100)
  nochange_sims$simtype = "nochange"

  return(nochange_sims)

}

#'  Simulate dynamics repeating begin size structure for end
#'
#' @param dat matss style
#'
#' @return sims drawn using begin ss for end (both actual and sim)
#' @export
#'
make_nosizechange_sims <- function(dat) {
  dat_gmms <- rwar::construct_sampling_gmm(dat, n_isd_draws = 10)

  nosizechange_gmms <- dat_gmms
  nosizechange_gmms$end <- nosizechange_gmms$begin %>% mutate(timeperiod = "end")

  nosizechange_sims <- rwar::draw_communities_wrapper(dat, sampling_gmms = nosizechange_gmms, ndraws = 100)

  nosizechange_sims$simtype = "nosizechange"
  return(nosizechange_sims)
}


#' Draw sims according to actual dynamics
#'
#' Draws both actual and sim sims.
#'
#' @param dat matss style
#'
#' @return sims drawn as normal
#' @export
make_actual_sims <- function(dat){
  dat_gmms <- rwar::construct_sampling_gmm(dat, n_isd_draws = 10)
  sims <- rwar::draw_communities_wrapper(dat, sampling_gmms = dat_gmms, ndraws = 100)
  sims$simtype = "actual"
  return(sims)
}

#' Summarize across sims
#'
#' @param sims sims df
#'
#' @return sims df summarized to mean energy and biomass for each year*source
#' @export
#' @importFrom dplyr filter group_by summarize n ungroup
summarize_sims <- function(sims) {

  sims <- sims %>% dplyr::filter(source != "raw") %>%
    dplyr::group_by(timeperiod, source, year, matssname,simtype) %>%
    dplyr::summarize(total_energy = mean(total_energy),
                     total_biomass = mean(total_biomass),
                     ndraws = dplyr::n()) %>%
    dplyr::ungroup()

  return(sims)

}


loo_select <- function(some_models, some_compares) {

  # Select the simplest model within 1 se of the best model

  winners <- some_compares %>%
    mutate(model_complexity = ifelse(grepl("full", model), 3,
                                     ifelse(grepl("source", model), 2, 1))) %>%
    arrange(matssname, currency, simtype, rank) %>%
    mutate(in_one_se = (elpd_diff + se_diff ) >= 0) %>%
    filter(in_one_se) %>%
    group_by(currency) %>%
    arrange(model_complexity) %>%
    mutate(model_rank = dplyr::row_number()) %>%
    ungroup() %>%
    filter(model_rank == 1)

  biomass_winner <- winners %>%
    filter(currency == "biomass") %>%
    select(model) %>%
    as.character()

  energy_winner <- winners %>%
    filter(currency == "energy") %>%
    select(model) %>%
    as.character()

  winner_mods <- list(
    te_winner = some_models$te_brms[[energy_winner]],
    tb_winner = some_models$tb_brms[[biomass_winner]],
    matssname = some_models$matssname,
    simtype = some_models$simtype,
    winner_info = list(te_winner = energy_winner,
                       tb_winner = biomass_winner)
  )

  return(winner_mods)
}

winner_draws <- function(loo_results) {

  te_draws <- tidybayes::tidy_draws(loo_results$te_winner) %>%
    dplyr::mutate(currency = "energy", modtype = loo_results$winner_info$te_winner)

  tb_draws <- tidybayes::tidy_draws(loo_results$tb_winner) %>%
    dplyr::mutate(currency = "biomass", modtype = loo_results$winner_info$tb_winner)

  all_draws <- dplyr::bind_rows(te_draws, tb_draws) %>%
    dplyr::mutate(matssname = loo_results$matssname,
                  simtype = loo_results$simtype)

  return(all_draws)
}

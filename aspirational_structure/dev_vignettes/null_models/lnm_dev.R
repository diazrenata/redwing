library(dplyr)
library(BBSsize)
library(rwar)
g <- MATSS::get_bbs_route_region_data(route = 224, region  =3)
local_null_model <- function(ts_dat, null_mod_seed = 1989, sim_index = 1, begin_years = 1988:1992, end_years = 2014:2018, isd_seed = 1989, n_isd_draws = 10, ndraws = 100) {

  if(is.null(null_mod_seed)) {
    set.seed(NULL)
    null_mod_seed <- sample.int(1000000000, 1)
  }

  shuffled_dat <- shuffle_local(ts_dat = ts_dat, null_mod_seed = null_mod_seed)

  null_sims <- ssims_wrapper(shuffled_dat, initial_isd_seed_gmm =isd_seed, initial_draw_seed = null_mod_seed, n_isd_draws = n_isd_draws, ndraws = ndraws, simtype = "actual")

  results <- null_sims %>%
    dplyr::mutate(
      null_mod_type = "local",
      null_mod_seed = null_mod_seed,
      sim_index = sim_index
    )


  results
}


nm <- local_null_model(g, n_isd_draws = 2, ndraws= 2)

nm2 <- local_null_model(g, n_isd_draws = 2, ndraws= 2, null_mod_seed = 1990)
nm3 <- local_null_model(g, n_isd_draws = 2, ndraws= 2, null_mod_seed = 1990)


local_null_model_wrapper <- function(ts_dat, n_null_model_sims = 100, begin_years = 1988:1992, end_years = 2014:2018, isd_seed = 1989, n_isd_draws = 10, ndraws = 100, initial_null_model_seed = 1989) {

  null_model_seeds <- initial_null_model_seed:(initial_null_model_seed + n_null_model_sims - 1)

  repeated_nulls <- list()

  for(i in 1:n_null_model_sims) {

    repeated_nulls[[i]] <- local_null_model(ts_dat, null_mod_seed = null_model_seeds[i], sim_index = i, begin_years = begin_years, end_years = end_years, isd_seed = isd_seed, ndraws = ndraws, n_isd_draws = n_isd_draws)
  }

  results <- dplyr::bind_rows(repeated_nulls)

  results

}

nm4 <- local_null_model_wrapper(g, n_null_model_sims = 5, n_isd_draws = 2, ndraws = 2)

ggplot(nm4, aes(year, total_biomass, color = source)) + geom_point()


a_brm <- brm_y

# Get all draws from the posterior and get just the terms we want
td <- tidybayes::tidy_draws(a_brm) %>%
  #  select_at(vars(starts_with("b"))) %>%
  dplyr::mutate(rowindex = dplyr::row_number()) #%>% # and get a row index to keep draws together, I'm not sure if this matters but I'll do it
# dplyr::filter(rowindex > max(rowindex) / 2) # remove warmup

td_ests <- td %>%
  dplyr::rename(year_sourcesim = `b_year:sourcesim`) %>%
  dplyr::select(rowindex, b_Intercept, b_year, b_sourcesim, year_sourcesim) %>%
  dplyr::group_by_all() %>%
  dplyr::mutate(
    estimated_actual_begin = sum(b_Intercept, (1988 * b_year)), # estimated beginning value
    estimated_actual_end = sum(b_Intercept, (2018 * b_year)), # estimated end value
    estimated_sim_begin = sum(b_Intercept, b_sourcesim, ((b_year + year_sourcesim) * 1988)), # estimated beginning value from sims. we expect this to be equal to the estimated beginning value, any change is just sampling error.
    estimated_sim_end = sum(b_Intercept, b_sourcesim, ((b_year + year_sourcesim) * 2018)),
    estimated_actual_change_ratio = (estimated_actual_end - estimated_actual_begin) / estimated_actual_begin, # this is a measure of the magnitude of the change from beginning to end. the sign is going to be increase (positive) or decrease. the magnitude is the % increase. so .1 = added 10% of starting (biomass or energy) to get to the end. -.2 = lost 20% of starting (biomass or energy) between begin and end.
    estimated_sim_change_ratio = (estimated_sim_end - estimated_sim_begin) / estimated_sim_begin, # same measure but having drawn the end values using the beginning isd. this is the amount of change expected due only to changes in the numbers of individuals observed in each time period. by comparing estimated_actual_change_ratio to estimated_sim_change_ratio, I believe we get an estimate of both the significance and magnitude of decoupling of (biomass or energy) and numerical abundance due to changes in the size spectrum.
    estimated_actual_change = estimated_actual_end - estimated_actual_begin, # the "slope" assuming x = 0 or 1 for begin or end. aka the absolute change from end to begin.
    estimated_sim_change = estimated_sim_end - estimated_sim_begin, # absolute change from end to begin due to abundance change
    estimated_change_ratio_deviation = estimated_actual_change_ratio - estimated_sim_change_ratio, # deviation of change ratios from 1:1
    estimated_change_deviation = estimated_actual_change - estimated_sim_change # deviation of actual change from 1:1

  )





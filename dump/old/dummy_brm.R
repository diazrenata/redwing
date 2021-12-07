notrend <- data.frame(abundance = c(120, 110, 115, 130, 120, 115, 130, 120, 125, 120), biomass = NA, energy = NA, timeperiod = c(rep("begin", times = 5), rep("end", times = 5)), dat = "nt") %>%
  mutate(biomass = (rnorm(60, 2, n = 10) * abundance)) %>%
  mutate(energy = estimate_b(biomass))

flattrend <- data.frame(abundance =c(120, 110, 115, 130, 120, 115, 130, 120, 125, 120), biomass = NA, energy = NA, timeperiod = c(rep("begin", times = 5), rep("end", times = 5)), dat = "ft") %>%
  mutate(biomass = ( rnorm(60, 2, n = 10) * abundance)) %>%
  mutate(biomass = ifelse(
    timeperiod == "begin",
    2 * biomass,
    biomass
  )) %>%
  mutate(energy = estimate_b(biomass))

trendtogether <- data.frame(abundance = c(sample(100:120, size = 5), sample(160:170, size = 5)), biomass = NA, energy = NA, timeperiod = c(rep("begin", times = 5), rep("end", times = 5)), dat = "tt") %>%
  mutate(biomass = ( rnorm(60, 2, n = 10) * abundance)) %>%
  mutate(energy = estimate_b(biomass))


trenddifferent <- data.frame(abundance = c(sample(100:120, size = 5), sample(160:170, size = 5)), biomass = NA, energy = NA, timeperiod = c(rep("begin", times = 5), rep("end", times = 5)), dat = "td") %>%
  mutate(biomass = ((rnorm(60, 2, n = 10) * abundance))) %>%
  mutate(biomass = ifelse(
    timeperiod == "begin",
    2 * biomass,
    biomass
  )) %>%
  mutate(energy = estimate_b(biomass))

trendtoflat <- data.frame(abundance = c(sample(100:120, size = 5), sample(160:170, size = 5)), biomass = NA, energy = NA, timeperiod = c(rep("begin", times = 5), rep("end", times = 5)), dat = "tf") %>%
  mutate(biomass = ((rnorm(60, 2, n = 10) *  c(sample(100:120, size = 10, replace = T))))) %>%
  mutate(energy = estimate_b(biomass))


alldat <- bind_rows(flattrend, notrend, trendtogether, trenddifferent, trendtoflat) %>%
  group_by_all() %>%
  mutate(mean_biomass = biomass / abundance,
         mean_energy = energy / abundance) %>%
  ungroup() %>%
  pivot_longer(-c(timeperiod, dat), names_to = "currency", values_to = "value")

alldat_scaled <- alldat %>%
  group_by(currency, dat) %>%
  mutate(val = scale(sqrt(value))) %>%
  ungroup()

ggplot(alldat_scaled, aes(timeperiod, val)) + geom_point() + facet_grid(cols = vars(dat), rows = vars(currency), scales = "free")

extract_brm_fit <- function(scaled_svs) {

  cap_brm1 <- brm(val ~ timeperiod * currency, scaled_svs)

  lower95 <- function(vect) {
    return(as.numeric(quantile(vect, .025)))
  }
  upper95 <- function(vect) {
    return(as.numeric(quantile(vect, .975)))
  }

  ests1 <- tidybayes::spread_draws(cap_brm1, b_timeperiodend,`b_timeperiodend:currencybiomass`, `b_timeperiodend:currencyenergy`) %>%
    dplyr::mutate(
      slope_abund = b_timeperiodend,
      slope_biomass = b_timeperiodend + `b_timeperiodend:currencybiomass`,
      slope_energy = b_timeperiodend + `b_timeperiodend:currencyenergy`
    ) #%>%
    #select(-c(.chain, .draw, .iteration)) %>%
    #summarize_all(.funs = list(
    #   mean = mean,
    #   lower95 = lower95,
    #   upper95 = upper95
    # ))

  ests1

}

pd <- function(datname, alldat) {
  dplyr::filter(alldat, dat == datname)
}

dats <- unique(alldat_scaled$dat)

dats_scaled <- lapply(dats,pd, alldat = alldat_scaled)

dats_ests <- lapply(dats_scaled, extract_brm_fit)

names(dats_ests) = dats

dats_ests_df <- bind_rows(dats_ests, .id = "dat")

ggplot(dats_ests_df, aes(b_timeperiodend)) + geom_density() + geom_vline(xintercept = 0) + facet_wrap(vars(dat))


ggplot(dats_ests_df, aes(`b_timeperiodend:currencybiomass`)) + geom_density() + geom_vline(xintercept = 0) + facet_wrap(vars(dat))


ggplot(dats_ests_df, aes(`b_timeperiodend:currencyenergy`)) + geom_density() + geom_vline(xintercept = 0) + facet_wrap(vars(dat))


ggplot(dats_ests_df, aes(slope_abund)) + geom_density() + geom_vline(xintercept = 0) + facet_wrap(vars(dat))


ggplot(dats_ests_df, aes(slope_biomass)) + geom_density() + geom_vline(xintercept = 0) + facet_wrap(vars(dat))

ggplot(dats_ests_df, aes(slope_energy)) + geom_density() + geom_vline(xintercept = 0) + facet_wrap(vars(dat)) + geom_density(aes(x = slope_abund), color = "green")



library(portlar)
library(dplyr)
library(ggplot2)


alltime <- portalr::summarise_individual_rodents(clean = TRUE, type = "Granivores", time = "longterm", fillweight = TRUE, min_traps = 45, min_plots = 22) %>%
  filter(!is.na(species))

obs_per_period <- alltime %>%
  group_by(species, period, year) %>%
  summarize(nobs = dplyr::n()) %>%
  ungroup() %>%
  full_join(as.data.frame(expand.grid(species = unique(alltime$species), period = unique(alltime$period)))) %>%
  group_by_all() %>%
  mutate(nobs = ifelse(is.na(nobs), 0, nobs)) %>%
  ungroup() %>%
  group_by(species) %>%
  summarize(prop_obs = mean(nobs > 0))  %>%
  mutate(transient = prop_obs < 1/3,
         core = prop_obs > 2/3) %>%
  ungroup()

ggplot(obs_per_period, aes(prop_obs)) +
  geom_histogram() +
  geom_vline(xintercept = .66)


obs_per_year <- alltime %>%
  group_by(species, year) %>%
  summarize(nobs = dplyr::n()) %>%
  ungroup() %>%
  full_join(as.data.frame(expand.grid(species = unique(alltime$species), year = unique(alltime$year)))) %>%
  group_by_all() %>%
  mutate(nobs = ifelse(is.na(nobs), 0, nobs)) %>%
  ungroup() %>%
  group_by(species) %>%
  summarize(prop_obs = mean(nobs > 0))  %>%
  mutate(transient = prop_obs < 1/3,
         core = prop_obs > 2/3) %>%
  ungroup()

ggplot(obs_per_year, aes(prop_obs)) +
  geom_histogram() +
  geom_vline(xintercept = 1/3)

not_transient <- obs_per_period %>%
  filter(!transient)

notransients <- alltime %>%
  filter(species %in% not_transient$species)

good_control_plots <- c(11, 14, 22,2)


white_dat <- filter(alltime, as.numeric(year) %in% c(1978:1982, 2000:2004), plot %in% good_control_plots) %>%
  mutate(scientificName = as.character(species),
         treatment = ifelse(year > 1985, "end", "start"))

white_splist <- white_dat %>%
  group_by(scientificName) %>%
  summarize(mean_wgt = mean(wgt),
            sd_to_use = sd(wgt),
            nind = dplyr::n()) %>%
  ungroup()

white_lm <- lm(log(sd_to_use ^ 2) ~ log(mean_wgt), data = filter(white_splist, !is.na(sd_to_use)))

white_coef = coef(white_lm)

white_splist <- mutate(white_splist,
                       log_var_est = white_coef[[1]] + (log(mean_wgt) * white_coef[[2]])) %>%
  mutate(var_est = exp(log_var_est)) %>%
  group_by_all() %>%
  mutate(sd_to_use = ifelse(nind < 2, sqrt(var_est), sd_to_use),
         sd_estimated = nind < 2) %>%
  ungroup()

white_counts <- white_dat %>%
  group_by(treatment, scientificName) %>%
  summarize(abund = dplyr::n()) %>%
  ungroup() %>%
  mutate(oldName = scientificName,
         sim = -99) %>%
  rename(siteID = treatment)




write.csv(white_counts, here::here("portal_nulls", "white.csv"), row.names = F)
write.csv(white_splist, here::here("portal_nulls", "white_splist.csv"), row.names = F)

#### Remove transients ####


white_dat_not <- filter(notransients, as.numeric(year) %in% c(1978:1982, 2000:2004), plot %in% good_control_plots) %>%
  mutate(scientificName = as.character(species),
         treatment = ifelse(year > 1985, "end", "start"))


white_not_splist <- white_dat_not %>%
  group_by(scientificName) %>%
  summarize(mean_wgt = mean(wgt),
            sd_to_use = sd(wgt)) %>%
  ungroup()


white_not_counts <- white_dat_not %>%
  group_by(treatment, scientificName) %>%
  summarize(abund = dplyr::n()) %>%
  ungroup() %>%
  mutate(oldName = scientificName,
         sim = -99) %>%
  rename(siteID = treatment)



write.csv(white_not_counts, here::here("portal_nulls", "white_not.csv"), row.names = F)
write.csv(white_not_splist, here::here("portal_nulls", "white_not_splist.csv"), row.names = F)



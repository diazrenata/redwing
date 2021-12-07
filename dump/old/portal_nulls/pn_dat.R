library(portlar)
library(dplyr)
library(ggplot2)


alltime <- portalr::summarise_individual_rodents(clean = TRUE, type = "Granivores", time = "longterm", fillweight = TRUE, min_traps = 45, min_plots = 22) %>%
  filter(!is.na(species))


these_obs <- alltime %>%
  filter(as.numeric(year) %in% c(1980:2008))

obs_per_period <- these_obs %>%
  group_by(species, period, year) %>%
  summarize(nobs = dplyr::n()) %>%
  ungroup() %>%
  full_join(as.data.frame(expand.grid(species = unique(these_obs$species), period = unique(these_obs$period)))) %>%
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


obs_per_year <- these_obs %>%
  group_by(species, year) %>%
  summarize(nobs = dplyr::n()) %>%
  ungroup() %>%
  full_join(as.data.frame(expand.grid(species = unique(these_obs$species), year = unique(these_obs$year)))) %>%
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

notransients <- these_obs %>%
  filter(species %in% not_transient$species)


#' 1977-85:
#' Plots 3, 19: Krats removed
#' Plots 11, 14: Unmanip
#' Plots 8, 12: P. rugosus removed
#' Plots 15, 21: Krats and P. rugosus removed

eighties <- filter(alltime,
                   as.numeric(year) %in% c(1980:1984),
                   plot %in% c(3, 19, 11, 14, 8, 12, 15, 21)) %>%
  mutate(scientificName = as.character(species))

eighties_splist <- eighties %>%
  group_by(scientificName) %>%
  summarize(mean_wgt = mean(wgt),
            sd_to_use = sd(wgt)) %>%
  ungroup()


eighties_counts <- eighties %>%
  group_by(treatment, scientificName) %>%
  summarize(abund = dplyr::n()) %>%
  ungroup() %>%
  mutate(oldName = scientificName,
         sim = -99) %>%
  rename(siteID = treatment)


# 2000-2004
# 2, 11, 14, 22: Unmanip
# 6, 15, 18, 21: Krats removed

thous <- filter(alltime,
                as.numeric(year) %in% c(2005:2008),
                plot %in% c(2, 11, 14, 22, 6, 15, 18, 21)) %>%
  mutate(scientificName = as.character(species))

thous_splist <- thous %>%
  group_by(scientificName) %>%
  summarize(mean_wgt = mean(wgt),
            sd_to_use = sd(wgt),
            nind = dplyr::n()) %>%
  ungroup()

thous_lm <- lm(log(sd_to_use ^ 2) ~ log(mean_wgt), data = filter(thous_splist, !is.na(sd_to_use)))

thous_coef = coef(thous_lm)

thous_splist <- mutate(thous_splist,
                      log_var_est = thous_coef[[1]] + (log(mean_wgt) * thous_coef[[2]])) %>%
  mutate(var_est = exp(log_var_est)) %>%
  group_by_all() %>%
  mutate(sd_to_use = ifelse(nind < 2, sqrt(var_est), sd_to_use),
         sd_estimated = nind < 2) %>%
  ungroup()

thous_counts <- thous %>%
  group_by(treatment, scientificName) %>%
  summarize(abund = dplyr::n()) %>%
  ungroup() %>%
  mutate(oldName = scientificName,
         sim = -99) %>%
  rename(siteID = treatment)



write.csv(eighties_counts, here::here("portal_nulls", "eighties.csv"), row.names = F)
write.csv(eighties_splist, here::here("portal_nulls", "eighties_splist.csv"), row.names = F)
write.csv(thous_counts, here::here("portal_nulls", "thous.csv"), row.names =F)
write.csv(thous_splist, here::here("portal_nulls", "thous_splist.csv"), row.names = F)


thous_splist_manip <- thous_splist %>%
  group_by_all() %>%
  mutate(mean_wgt = ifelse(scientificName == "PB", 40, mean_wgt),
         sd_to_use = ifelse(scientificName == "PB", 5.8, sd_to_use)) %>%
  ungroup()
write.csv(thous_splist_manip, here::here("portal_nulls", "thous_splist_MANIP.csv"), row.names = F)


#### Remove transients ####
#' 1977-85:
#' Plots 3, 19: Krats removed
#' Plots 11, 14: Unmanip
#' Plots 8, 12: P. rugosus removed
#' Plots 15, 21: Krats and P. rugosus removed

eighties_not <- filter(notransients,
                   as.numeric(year) %in% c(1980:1984),
                   plot %in% c(3, 19, 11, 14, 8, 12, 15, 21)) %>%
  mutate(scientificName = as.character(species))

eighties_not_splist <- eighties_not %>%
  group_by(scientificName) %>%
  summarize(mean_wgt = mean(wgt),
            sd_to_use = sd(wgt)) %>%
  ungroup()


eighties_not_counts <- eighties_not %>%
  group_by(treatment, scientificName) %>%
  summarize(abund = dplyr::n()) %>%
  ungroup() %>%
  mutate(oldName = scientificName,
         sim = -99) %>%
  rename(siteID = treatment)


# 2000-2004
# 2, 11, 14, 22: Unmanip
# 6, 15, 18, 21: Krats removed

thous_not <- filter(notransients,
                as.numeric(year) %in% c(2005:2008),
                plot %in% c(2, 11, 14, 22, 6, 15, 18, 21)) %>%
  mutate(scientificName = as.character(species))

thous_not_splist <- thous_not %>%
  group_by(scientificName) %>%
  summarize(mean_wgt = mean(wgt),
            sd_to_use = sd(wgt),
            nind = dplyr::n()) %>%
  ungroup()


thous_not_counts <- thous_not %>%
  group_by(treatment, scientificName) %>%
  summarize(abund = dplyr::n()) %>%
  ungroup() %>%
  mutate(oldName = scientificName,
         sim = -99) %>%
  rename(siteID = treatment)



write.csv(eighties_not_counts, here::here("portal_nulls", "eighties_not.csv"), row.names = F)
write.csv(eighties_not_splist, here::here("portal_nulls", "eighties_not_splist.csv"), row.names = F)
write.csv(thous_not_counts, here::here("portal_nulls", "thous_not.csv"), row.names =F)
write.csv(thous_not_splist, here::here("portal_nulls", "thous_not_splist.csv"), row.names = F)


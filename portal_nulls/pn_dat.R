library(portlar)
library(dplyr)
library(ggplot2)


alltime <- portalr::summarise_individual_rodents(clean = TRUE, type = "Granivores", time = "longterm", fillweight = TRUE, min_traps = 45, min_plots = 22) %>%
  filter(!is.na(species))

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
                as.numeric(year) %in% c(2000:2004),
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

library(neonUtilities)
library(dplyr)
library(ggplot2)

#rata <- loadByProduct("DP1.10072.001")
zipsByProduct("DP1.10072.001", release = "RELEASE-2021", savepath = here::here("neon_mammals"))
stackByTable(here::here("neon_mammals", "filesToStack10072"))


site_plot_nights <- mam_perplotnight %>%
  filter(is.na(remarks)) %>%
  mutate(collectYear = format.Date(collectDate, "%Y"),
         collectMonth = format.Date(collectDate, "%m")) %>%
  group_by(siteID, collectYear) %>%
  summarize(nMonths = length(unique(collectMonth)),
            listMonths = toString(unique(collectMonth))) %>%
  ungroup() %>%
  filter(nMonths >= 4) %>%
  filter(as.integer(collectYear) %in% c(2015:2019)) %>%
  group_by(siteID) %>%
  summarize(nyears= length(unique(collectYear))) %>%
  filter(nyears >= 4)

complete_sites <- site_plot_nights$siteID




trapped_individuals <- mam_pertrapnight %>%
  filter(siteID %in% complete_sites) %>%
  mutate(collectYear = format.Date(collectDate, "%Y")) %>%
  filter(as.integer(collectYear) %in% c(2015:2019)) %>%
  filter(trapStatus == "5 - capture") %>%
  filter(taxonRank == "species") %>%
  filter(fate != "nontarget") %>%
  filter(!is.na(weight))%>%
  group_by_all() %>%
  mutate(genus = unlist(strsplit(scientificName, " "))[[1]]) %>%
  ungroup()


trapped_genuses <- select(trapped_individuals, genus) %>%
  distinct()


# via some googling and the list here https://www.ecography.org/sites/ecography.org/files/appendix/ecog-03641.pdf
not_target <- c("Tamias",
                "Sorex",
                "Blarina",
                "Mustela",
                "Cryptotis",
                "Glaucomys",
                "Tamiasciurus",
                "Synaptomys",
                "Sylvilagus",
                "Ictidomys",
                "Spermophilus",
                "Onychomys",
                "Ammospermophilus")

trapped_individuals <- trapped_individuals %>%
  filter(!(genus %in% not_target))


site_dat <- trapped_individuals %>%
  select(siteID, decimalLatitude, decimalLongitude, geodeticDatum) %>%
  distinct() %>%
  group_by(siteID) %>%
  mutate(recordnb = row_number()) %>%
  filter(recordnb == 1) %>%
  ungroup()

ind_dat <- trapped_individuals %>%
  select(siteID, scientificName, weight)

species_dat <- ind_dat %>%
  group_by(scientificName) %>%
  summarize(mean_wgt = mean(weight),
            sd_wgt = sd(weight),
            nind = dplyr::n()) %>%
  ungroup()%>%
  mutate(var = sd_wgt^2) %>%
  mutate(log_m = log(mean_wgt),
         log_var = log(var))

species_sd_dat <- species_dat %>%
  filter(nind > 5)

sd_lm <- lm(species_sd_dat, formula = log_var ~ log_m)

coef(sd_lm)

species_dat <- mutate(species_dat,
                    log_var_est = -3.256945 + (log_m * 2.109872)) %>%
  mutate(var_est = exp(log_var_est)) %>%
  group_by_all() %>%
  mutate(sd_to_use = ifelse(nind < 5, sqrt(var_est), var),
         sd_estimated = nind < 5) %>%
  ungroup()


ggplot(ind_dat, aes(log(weight))) +
  geom_density() +
  facet_wrap(vars(siteID))

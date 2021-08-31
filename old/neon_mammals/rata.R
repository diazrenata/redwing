####' Provisional script for downloading & processing NEON small mammal data.
####' Renata Diaz, July 15, 2021
####'

library(neonUtilities)
library(dplyr)
library(ggplot2)

#### Download and prep data per neonUtilities ####
# Don't run; takes a long time
# Downloaded 7/14/2021 by RMD
# This is the citation from the NEON data portal site. Note I downloaded using neonUtilities 2.1.0.
# NEON is released under CC0 1.0, no rights reserved. Cite and use appropriately. https://www.neonscience.org/data-samples/data-policies-citation
# NEON (National Ecological Observatory Network). Small mammal box trapping, RELEASE-2021 (DP1.10072.001). https://doi.org/10.48443/j1g9-2j27. Dataset accessed from https://data.neonscience.org on July 14, 2021
# zipsByProduct("DP1.10072.001", release = "RELEASE-2021", savepath = here::here("neon_mammals", "provisional_rata_raw"))
# stackByTable(here::here("neon_mammals", "provisional_rata_raw", "filesToStack10072"))

#### Get only captures ####
# mam_pertrapnight is the primary captures data file. As downloaded it is 450.3MB, too big for git. Filtering out non-captures reduces it to 63.4 MB.
# mam_pertrapnight <- read.csv(here::here("neon_mammals", "provisional_rata_raw", "filesToStack10072", "stackedFiles", "mam_pertrapnight.csv"))
# mam_pertrapnight_captures <- mam_pertrapnight %>%
#  filter(trapStatus == "5 - capture")
#
#write.csv(mam_pertrapnight_captures, here::here("neon_mammals", "provisional_rata_use", "raw_captures.csv"), row.names = F)


#### Provisional filtering ####
# May want to revisit these decisions, but trying to explain my logic.

# Plot trapping history data
# I manually went through all the "remarks" in mam_perplotnight to try and partition ones that would impact data quality (e.g. "traps not set", "bear disturbed many traps") vs those that aren't relevant ("blood samples not collected"). In particular, animal disturbances, incompletely set plots, "limited" or "priority" processing, or grid/bout closures/cancellations, I flagged as 1. Cattle present, inclement weather (not referencing an impact on sampling), protocol updates I left as 0s.
mam_perplotnight_qf <- read.csv(here::here("neon_mammals", "provisional_rata_use", "plot_history_qf.csv"))

site_plot_nights <- mam_perplotnight_qf %>%
  group_by_all() %>%
  mutate(remarksQF_RMD = ifelse(is.na(remarksQF_RMD), 0, remarksQF_RMD)) %>%
  ungroup() %>%
  filter(remarksQF_RMD != 1)


#### Get trapping bouts ####
# collectDates within 2 weeks of each other presumed to constitute the same trapping bout. This corresponds roughly to months, but sometimes the months turn over within a bout (6/30, 7/1).
# Will remove records of individuals recaptured within the same trapping bout.


site_plot_bouts <- site_plot_nights %>%
  mutate(collectDate = as.Date(collectDate, format = "%m/%d/%y"))

all_sites <- unique(site_plot_bouts$siteID)

bouts <- list()

for(i in 1:length(all_sites)) {

  this_site <- filter(site_plot_bouts, siteID == all_sites[i])

  bout_numbers = c(1)
  bout_startdates = this_site$collectDate[1]


  this_bout_number = 1
  this_bout_startdate = this_site$collectDate[1]


  for(j in 2:nrow(this_site)) {

    if(this_site$collectDate[j] - this_bout_startdate > 14) {

      this_bout_number = this_bout_number + 1
      this_bout_startdate = this_site$collectDate[j]

    }

    bout_numbers <- c(bout_numbers, this_bout_number)
    bout_startdates <- c(bout_startdates, this_bout_startdate)

  }

  this_site$bout_number <- bout_numbers
  this_site$bout_startdates <- bout_startdates

  bouts[[i]] <- this_site
}

sites_bouts <- bind_rows(bouts)  %>%
  mutate(boutMonth = format.Date(bout_startdates, "%m"),
         boutYear = format.Date(bout_startdates, "%Y"))

write.csv(sites_bouts, here::here("neon_mammals", "provisional_rata_use", "bout_history.csv"), row.names =F)


#### Filter sites and bouts ####

sites_filtered <- sites_bouts %>%
  group_by(siteID, boutYear) %>%
  summarize(nMonths = length(unique(boutMonth)),
            listMonths = toString(unique(boutMonth))) %>%
  ungroup() %>%
  filter(nMonths >= 3) %>% # Want at least 3 census bouts in each year
  filter(as.integer(boutYear) %in% c(2015:2019)) %>% # Some designs were changed from 2013-2015 and after 2015; see readme. 2020 was 2020.
  group_by(siteID) %>%
  summarize(nyears= length(unique(boutYear))) %>%
  filter(nyears >= 4) # In each of 4 years from 2015-2019

complete_sites <- sites_filtered$siteID

keep_bouts <- filter(sites_bouts, siteID %in% complete_sites, boutYear %in% c(2015:2019)) %>%
  mutate(keep = 1)

#### Load captures and filter to desired bouts x sites ####

raw_captures <- read.csv(here::here("neon_mammals", "provisional_rata_use", "raw_captures.csv"))

raw_captures <- raw_captures %>%
  mutate(collectDate = as.Date(collectDate, format = "%Y-%m-%d"))

captures_bouts <- raw_captures  %>%
  left_join(keep_bouts, by = c("nightuid", "plotID", "siteID", "domainID", "namedLocation", "collectDate", "publicationDate", "release")) %>%
  filter(keep == 1)

#### Filter captures to target species, fully identified  ####

captured_individuals <- captures_bouts %>%
  filter(taxonRank == "species") %>%
  filter(fate != "nontarget") %>%
#  filter(!is.na(weight))%>% # Of these, 10000 are unweighed. We can interpolate those.
  group_by_all() %>%
  mutate(genus = unlist(strsplit(scientificName, " "))[[1]]) %>%
  ungroup()


trapped_genuses <- select(captured_individuals, genus) %>%
  distinct() %>%
  arrange(genus)


# via some googling and the list here https://www.ecography.org/sites/ecography.org/files/appendix/ecog-03641.pdf
not_target <- c("Ammospermophilus", #squirrel
                "Blarina", #shrew
                "Cryptotis", #shrew
                "Glaucomys", # flying squirrel
                "Ictidomys", # ground squirrel
                "Lepus", # hares
                "Mustela", # mustelids
                # keeping ochotona (pikas)
                "Onychomys", # grasshopper mice
                "Sciurus", # squirrels
                "Sorex", # shrew
                "Spermophilus", # squirrels
                "Sylvilagus", # bunnies
                "Synaptomys", # lemmings
                "Tamias", # chipmunks
                "Tamiasciurus", # chipmunks
                "Thomomys" # gopher
                )

captured_individuals <- captured_individuals %>%
  filter(!(genus %in% not_target))

#### Remove recaptures within a bout ####

recaptured <- captured_individuals  %>%
  group_by(domainID, siteID, bout_number, tagID, scientificName) %>%
  mutate(ncaps = dplyr::n(),
         which_cap = row_number()) %>%
  ungroup() %>%
  group_by_all() %>%
  mutate(remove_record = ifelse(tagID != "", ifelse(which_cap > 1, 1, 0), 0)) %>%
  ungroup()

recaps <- recaptured %>%
  filter(ncaps > 1) %>%
  arrange(domainID, siteID, bout_number, tagID)  %>%
  select(domainID, siteID, bout_number, tagID, ncaps, which_cap, remove_record)

captured_individuals <- captured_individuals %>%
  left_join(recaptured) %>%
  filter(remove_record != 1) # results in removing 17473 records

#### Remove juveniles ####
# Keeping subadults
# There are 1916 juveniles across all sites, 4% of all captures.
captured_individuals <- captured_individuals %>%
  filter(lifeStage != "juvenile")

ind_dat <- captured_individuals %>%
  select(siteID, scientificName, lifeStage, weight)

#### SD allometry ####
species_dat <- ind_dat %>%
  filter(!is.na(weight)) %>%
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

sd_coefs <- coef(sd_lm)

species_dat <- mutate(species_dat,
                  log_var_est = sd_coefs[[1]] + (log_m * sd_coefs[[2]])) %>%
  mutate(var_est = exp(log_var_est)) %>%
  group_by_all() %>%
  mutate(sd_to_use = ifelse(nind < 5, sqrt(var_est), sqrt(var)),
         sd_estimated = nind < 5) %>%
  ungroup()

#### Fill missing weights ####
set.seed(1989)
captured_individuals_interpolated <- captured_individuals %>%
  left_join(species_dat) %>%
  group_by_all() %>%
  mutate(interp_weight = ifelse(is.na(weight), rnorm(1, mean = mean_wgt, sd = sd_to_use), NA)) %>%
  mutate(weight_to_use = ifelse(is.na(weight), interp_weight, weight),
         weight_interpolated = !is.na(interp_weight)) %>%
  ungroup()

while(any(captured_individuals_interpolated$weight_to_use < 0)) {

  captured_individuals_interpolated <- captured_individuals_interpolated %>%
    group_by_all() %>%
    mutate(interp_weight = ifelse(weight_to_use < 0, rnorm(1, mean = mean_wgt, sd = sd_to_use), interp_weight)) %>%
    mutate(weight_to_use = ifelse(is.na(weight), interp_weight, weight),
           weight_interpolated = !is.na(interp_weight)) %>%
    ungroup()

}



write.csv(captured_individuals_interpolated, here::here("neon_mammals", "provisional_rata_use", "provisional_processed_rata.csv"), row.names = F)
write.csv(species_dat, here::here("neon_mammals", "provisional_rata_use", "provisional_sd_table.csv"))


ggplot(captured_individuals_interpolated, aes(log(weight_to_use))) +
  geom_density() +
  facet_wrap(vars(siteID))


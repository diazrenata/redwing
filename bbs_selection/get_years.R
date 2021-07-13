library(drake)
library(dplyr)
## Set up the cache and config
db <- DBI::dbConnect(RSQLite::SQLite(), here::here("drake-cache-years.sqlite"))
cache <- storr::storr_dbi("datatable", "keystable", db)
cache$del(key = "lock", namespace = "session")
loadd(year_coverage, cache = cache)

#year_coverage <- filter(year_coverage, location.routename %in% unique(year_coverage$location.routename)[1:10])

startyears <- c(1970:1990)

endyears <- c(2000:2020)

consec_starts <- list()

window = 6

nreq = 5

for(i in 1:length(startyears)) {

  for(j in 1:length(endyears)) {

    startcover <- startyears[i]:(startyears[i] + window)
    endcover <- endyears[j]:(endyears[j] - window)

    startcoverage <-  filter(year_coverage, year %in% startcover) %>%
      dplyr::group_by(route, region, location.bcr, location.statenum, location.routename) %>%
      dplyr::summarize(nyears_start = length(unique(year))) %>%
      dplyr::ungroup()

    endcoverage <- filter(year_coverage, year %in% endcover) %>%
      dplyr::group_by(route, region, location.bcr, location.statenum, location.routename) %>%
      dplyr::summarize(nyears_end = length(unique(year))) %>%
      dplyr::ungroup()

    fullcoverage <- left_join(startcoverage, endcoverage) %>%
      dplyr::group_by_all() %>%
      dplyr::mutate(nyears_start = ifelse(is.na(nyears_start), 0, nyears_start),
                    nyears_end = ifelse(is.na(nyears_end), 0, nyears_end)) %>%
      dplyr::mutate(complete_coverage = nyears_start >= nreq && nyears_end >= nreq) %>%
      dplyr::ungroup()

    fullcoverage_true <- filter(fullcoverage, complete_coverage)


    consec_starts[[length(consec_starts) + 1]] <- (data.frame(
      startyear = startyears[i],
      endyear = endyears[j],
      ncovered = sum(fullcoverage$complete_coverage, na.rm = T),
      nbcr_covered = length(unique(fullcoverage_true$location.bcr)),
      window = window,
      nreq = nreq
    ))
  }

}

starts2 <- bind_rows(consec_starts)


starts2 <- starts2 %>%
  mutate(timecovered = endyear - startyear)

library(ggplot2)

ggplot(filter(starts2, ncovered >0 ), aes(timecovered, ncovered, color = nbcr_covered)) +
  geom_point() +
  scale_color_viridis_c(option = "turbo", begin = .1, end = .9)

# For now I will work with 1988-2018. This gives 528 sites with complete coverage, or 1174 sites if we allow for 5/7 years in the start and end chunks.
# My data end in 2018.

startyear = 1988
endyear = 2018
window = 4
nreq = 5

startcover <- startyear:(startyear + window)
endcover <- endyear:(endyear - window)

startcoverage <-  filter(year_coverage, year %in% startcover) %>%
  dplyr::group_by(route, region, location.bcr, location.statenum, location.routename) %>%
  dplyr::summarize(nyears_start = length(unique(year))) %>%
  dplyr::ungroup()

endcoverage <- filter(year_coverage, year %in% endcover) %>%
  dplyr::group_by(route, region, location.bcr, location.statenum, location.routename) %>%
  dplyr::summarize(nyears_end = length(unique(year))) %>%
  dplyr::ungroup()

fullcoverage <- left_join(startcoverage, endcoverage) %>%
  dplyr::group_by_all() %>%
  dplyr::mutate(nyears_start = ifelse(is.na(nyears_start), 0, nyears_start),
                nyears_end = ifelse(is.na(nyears_end), 0, nyears_end)) %>%
  dplyr::mutate(complete_coverage = nyears_start >= nreq && nyears_end >= nreq) %>%
  dplyr::ungroup()

fullcoverage_true <- filter(fullcoverage, complete_coverage) %>%
  mutate(matssname = paste0("bbs_rtrg_", route, "_", region),
         window = window,
         nreq = nreq)
write.csv(fullcoverage_true, "working_routes.csv", row.names = F)

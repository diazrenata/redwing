library(rwar)
library(BBSsize)
library(dplyr)
library(ggplot2)

dat <- MATSS::get_bbs_route_region_data(route = 224, region = 3)
dat_isd <- simulate_isd_ts(dat, isd_seed = 1989)

annual_isds <- dat_isd$isd %>%
  filter(year %in% c(1988:1992, 2014:2018)) %>%
  mutate(fyear = as.factor(year),
         timeperiod = ifelse(year > 2000, "begin", "end"))

ggplot(annual_isds, aes(log(mass), color = fyear)) + geom_density()  + facet_wrap(vars(timeperiod))


annual_gmms <- list()

years <- c(1988:1992, 2014:2018)

for(i in 1:length(years)) {
  annual_gmms[[i]] <- construct_sampling_gmm(dat, 1, 1989, years[i], years[i])$begin  %>%
    mutate(timeperiod = NA,
           year = years[i]) %>%
    mutate(timeperiod = ifelse(year < 2000, "begin", "end"))
}

annual_gmms <- bind_rows(annual_gmms)%>%
  mutate(timeperiod = ifelse(year < 2000, "begin", "end"))

ggplot(annual_gmms, aes(mass, density, group = year, color= timeperiod)) + geom_line()

overall_gmm <- construct_sampling_gmm(dat, 1, 1989, years, years)$begin %>%
  mutate(timeperiod = 'overall')

ggplot(annual_gmms, aes(mass, density, group = year, color= timeperiod)) + geom_line() + geom_line(data = overall_gmm, aes(mass, density), inherit.aes = F)

all_possible <- as.data.frame(expand.grid(y1 = years, y2 = years)) %>%
  filter(y1 < y2) %>%
  rename(overlap = NA)

ol <- function(gmm1, gmm2) {
gmm1 <- select(gmm1, -year, -timeperiod)
  gmm2 <- rename(gmm2, density2 = density) %>%
    select(-year, -timeperiod)

  bothgmms <- left_join(gmm1, gmm2) %>%
    group_by_all() %>%
    mutate(mindensity = min(density, density2)) %>%
    ungroup()

  sum(bothgmms$mindensity)

}

for(i in 1:nrow(all_possible)) {

  g1 <- filter(annual_gmms, year == all_possible$y1[i])
  g2 <- filter(annual_gmms, year == all_possible$y2[i])

  all_possible$overlap[i] <- ol(g1, g2)
}

all_possible <- all_possible %>%
  mutate(intraperiod = y2 - y1 < 6,
         timegap = y2 - y1)

ggplot(all_possible, aes(intraperiod, overlap)) + geom_point() + geom_violin()

ggplot(all_possible, aes(timegap, overlap)) + geom_point()

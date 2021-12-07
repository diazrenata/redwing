library(BBSsize)
library(rwar)
library(dplyr)
library(ggplot2)

ts_comp <- granby
begin_years = 1988:1992
end_years = 2014:2018
#isd_seed = 1994

just_isd <- function(ts_comp, isd_seed = NULL) {
  simulate_isd_ts(ts_comp, isd_seed = isd_seed)$isd
}

ts_isds <- just_isd(ts_comp)

ts_isds_many <- (replicate(20, just_isd(ts_comp), simplify = F))
names(ts_isds_many) <- 1:20
ts_isds_many <- bind_rows(ts_isds_many, .id = "sim")


ts_isds_start <- filter(ts_isds, year %in% begin_years)
ts_isds_many_start <- filter(ts_isds_many, year %in% begin_years)

raw_summ<-ts_isds_many_start %>%
  group_by(sim) %>%
  summarize(totalmass = sum(mass),
            totaln = dplyr::n())

ts_isds_start_exp <- mutate(ts_isds_start, mass = exp(mass))


add_gmm2 <- function(isd, max_size = 15000, max_G = 15) {

  # isd <- isd %>%
  #   dplyr::mutate(logmass = log(mass))

  gmm <- fit_gmm(isd$mass, max_G)


  gmm_isd <- data.frame(mass = seq(0, (max_size), length.out = 100000))
  gmm_isd$dens <- predict(gmm, newdata = gmm_isd$mass)


  isd_gmm <- data.frame(
    mass = gmm_isd$mass,
    density = (gmm_isd$dens)/ sum(gmm_isd$dens)
  )

  return(isd_gmm)

}
one_start_gmm <- add_gmm2(ts_isds_start, max_size = (15000))
many_start_gmm <- add_gmm2(filter(ts_isds_many, year %in% begin_years))

ggplot(one_start_gmm, aes(mass, density)) + geom_line() + geom_line(data = many_start_gmm, color = 'blue')

sampled_isds <- replicate(n = 100, data.frame(mass = (sample(one_start_gmm$mass, size = nrow(filter(ts_isds, year %in% begin_years)), prob = one_start_gmm$density, replace = T))), simplify = F)

names(sampled_isds) <- 1:100

sampled_isds <- bind_rows(sampled_isds, .id = "sim2")



processed_sum <- sampled_isds %>%
  group_by(sim2) %>%
  summarize(totalmass = sum(mass),
            totaln = dplyr::n())
sampled_isds2 <- replicate(n = 100, data.frame(mass = (sample(one_start_gmm$mass, size = nrow(filter(ts_isds, year %in% begin_years)), prob = many_start_gmm$density, replace = T))), simplify = F)

names(sampled_isds2) <- 1:100

sampled_isds2 <- bind_rows(sampled_isds2, .id = "sim2")

processed_sum2 <- sampled_isds2 %>%
  group_by(sim2) %>%
  summarize(totalmass = sum(mass),
            totaln = dplyr::n())

summary(raw_summ$totalmass)
summary(processed_sum$totalmass)
summary(processed_sum2$totalmass)

summary(lm(raw_summ$totalmass ~ processed_sum$totalmass[1:50]))

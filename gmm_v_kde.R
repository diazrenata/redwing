library(BBSsize)
library(dplyr)
library(ggplot2)
library(mclust)
source(here::here("kdes.R"))

hartland <- hartland

hartland_isd <- simulate_isd_ts(hartland, censusyears = c(1994:1999))

hartland_gmm1 <- densityMclust(log(hartland_isd$isd$mass), G = c(1:15) )

hartland_gmm_isd <- data.frame(logmass = seq(0, log(15000), length.out = 1000))
hartland_gmm_isd$dens <- predict(hartland_gmm1, newdata = hartland_gmm_isd$logmass)

hartland_gmm_isd$scaleddens <- hartland_gmm_isd$dens / sum(hartland_gmm_isd$dens)

ggplot(hartland_gmm_isd, aes(logmass, dens)) + geom_line()

hartland_isd2<- simulate_isd_ts(hartland, censusyears = c(2014:2018))

hartland_gmm2 <- densityMclust(log(hartland_isd2$isd$mass), G = c(1:15) )

hartland_gmm_isd2 <- data.frame(logmass = seq(0, log(15000), length.out = 1000))
hartland_gmm_isd2$dens <- predict(hartland_gmm2, newdata = hartland_gmm_isd2$logmass)

hartland_gmm_isd2$scaleddens <- hartland_gmm_isd2$dens / sum(hartland_gmm_isd2$dens)

ggplot(hartland_gmm_isd2, aes(logmass, dens)) + geom_line()

gmm_change <- hartland_gmm_isd %>%
  rename(dens1 = dens,
         scaleddens1 = scaleddens) %>%
  left_join(hartland_gmm_isd2)

ggplot(gmm_change, aes(logmass, scaleddens1)) +
  geom_line() +
  geom_line(aes(y = scaleddens), color = "green")

ggplot(gmm_change, aes(logmass, scaleddens - scaleddens1)) + geom_line()

library(rwar)
library(BBSsize)
library(dplyr)
library(ggplot2)

dat = hartland

isds <- just_isd(dat, isd_seed= 1989)

raw_isd_begin <- filter(isds, year %in% 1994:1998)
raw_isd_end <- filter(isds, year %in% 2014:2018)

raw_isd_ks <- ks.test(raw_isd_begin$mass, raw_isd_end$mass)

raw_isd_ks$statistic

both_isds <- bind_rows(raw_isd_begin, raw_isd_end) %>%
  mutate(timeperiod = ifelse(year < 2000, "begin", "end"))

shuffles <- list()

set.seed(1989)

for(i in 1:200) {

  end_shuffle <- sample(both_isds$mass, size = nrow(raw_isd_end), replace = F)
  begin_shuffle <- sample(both_isds$mass, size = nrow(raw_isd_begin), replace = F)
  shuffle_ks <- ks.test(begin_shuffle, end_shuffle)

  shuffles[[i]] <- data.frame(
    i = i,
    d = shuffle_ks$statistic,
    p = shuffle_ks$p.value
  )
  }

shuffles <- bind_rows(shuffles)

hist(shuffles$d)
hist(shuffles$p)
raw_isd_ks$statistic
raw_isd_ks$p.value

ses(raw_isd_ks$statistic, shuffles$d)
percentile_score(raw_isd_ks$statistic, shuffles$d)

sampling_gmms <- construct_sampling_gmm(dat, 5, 1989, begin_years = 1994:1998)




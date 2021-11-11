library(BBSsize)
library(dplyr)
library(ggplot2)

dat <- granby
ssims <- rwar::ssims_wrapper(dat, "actual",n_isd_draws = 2, ndraws=2)

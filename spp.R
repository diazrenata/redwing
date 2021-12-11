library(rwar)
library(BBSsize)
library(dplyr)
library(ggplot2)

dat <- granby

slice_dat <- function(dataset, keep_years = c(1988:1992, 2014:2018)) {

  keep_rows <- which(dataset$covariates$year %in% keep_years)

  dat_out <- dataset

  dat_out$abundance <- dat_out$abundance[keep_rows, ]
  dat_out$covariates <- dat_out$covariates[keep_rows,]

  dat_out
}

resp_dat <- function(dataset, resp_seed = 1989) {
#
#   spTotalAbunds <- colSums(dataset$abundance)
#   spRelAbunds <- spTotalAbunds / sum(spTotalAbunds)

  spRelAbunds <- dataset$abundance/ rowSums(dataset$abundance)
  spRelAbunds <- colSums(spRelAbunds) / nrow(spRelAbunds)

  draw_year <- function(year_row, spRelAbunds) {
    as.data.frame(t(rmultinom(1, size  = sum(year_row), prob = spRelAbunds)))
  }

  set.seed(resp_seed)

  newAbund <- apply(dataset$abundance, MARGIN = 1, FUN = draw_year, spRelAbunds = spRelAbunds)
  set.seed(NULL)
  dataset$abundance <- bind_rows(newAbund)

  dataset
}

calc_sv <- function(isd) {

  sv_isd <- isd$isd %>%
    mutate(energy = estimate_b(mass)) %>%
    group_by(year) %>%
    summarize(abundance = dplyr::n(),
              biomass = sum(mass),
              energy = sum(energy)) %>%
    ungroup()

  bind_cols(sv_isd, isd$metadata$location)
}

dat_sliced <- slice_dat(dat)

dat_resp <- resp_dat(dat_sliced, resp_seed = 1977)

dat_isd <- simulate_isd_ts(dat_sliced, isd_seed = 1989)

dat_resp_isd <- simulate_isd_ts(dat_resp, isd_seed = 1989)

ggplot(dat_isd$isd, aes(mass, color = as.factor(year))) + geom_density() + scale_x_log10() + geom_density(data = dat_resp_isd$isd, linetype =2) + facet_wrap(vars(year))


real_sv <- calc_sv(dat_isd) %>%
  mutate(source = "real")
sim_sv <- calc_sv(dat_resp_isd) %>%
  mutate(source = "sim")

both_sv <- bind_rows(real_sv, sim_sv)  %>%
  mutate(fsource = as.factor(source))

ggplot(filter(both_sv), aes(year, biomass, color = source)) + geom_point()

lm(biomass ~ source * year, data = both_sv)
summary(lm(biomass ~ source * year, data = both_sv))


library(gratia)
load_mgcv()

biomass_gam <- gam(data = both_sv, biomass ~ s(year, k= 30) + s(year, by = fsource), family = tw())

summary(biomass_gam)

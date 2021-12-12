library(rwar)
library(BBSsize)
library(dplyr)
library(ggplot2)

dat <- MATSS::get_bbs_route_region_data(route = 19, region = 7)
#dat <- hartland
slice_dat <- function(dataset, keep_years = c(1988:2018)) {

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

dat_sliced <-slice_dat(dat)

dat_resp <- resp_dat(dat_sliced, resp_seed = 1989)

dat_isd <- simulate_isd_ts(dat_sliced, isd_seed = 1989)

dat_resp_isd <- simulate_isd_ts(dat_resp, isd_seed = 1989)

#ggplot(dat_isd$isd, aes(mass, color = as.factor(year))) + geom_density() + scale_x_log10() + geom_density(data = dat_resp_isd$isd, linetype =2) + facet_wrap(vars(year))


real_sv <- calc_sv(dat_isd) %>%
  mutate(source = "real")
sim_sv <- calc_sv(dat_resp_isd) %>%
  mutate(source = "sim")

both_sv <- bind_rows(real_sv, sim_sv)  %>%
  mutate(fsource = as.factor(source)) #%>%
  #filter(year %in% c(1988:2014))

ggplot(filter(both_sv), aes(year, biomass, color = source)) + geom_point()
#
# lm(energy ~ source * year, data = both_sv)
# summary(lm(energy ~ source * year, data = both_sv))
#
# ggplot(filter(both_sv), aes(year, energy, color = source)) + geom_point() + geom_line(data = both_sv %>% mutate(energy = predict(lm(energy ~ source * year, data = both_sv)
# )))


library(gratia)
load_mgcv()

biomass_gam <- gam(data = both_sv, formula = biomass ~  fsource + s(year, by = fsource, k = 30), method = "REML", select = T, family = tw())
biomass_gam2 <- gam(data = both_sv, formula = biomass ~  (fsource) + s(year, k = 30),  method = "REML", select = T, family = tw())

biomass_gam3 <- gam(data = both_sv, formula = biomass ~  s(year, k = 30), method = "REML", select = T, family = tw())

AIC(biomass_gam)
AIC(biomass_gam2)
AIC(biomass_gam3)

biomass_gam4 <- gam(data = both_sv, formula = biomass ~ year * source, method = "REML", select = T, family = tw())

biomass_gam5 <- gam(data = both_sv, formula = biomass ~ year, method = "REML", select = T, family = tw())

biomass_gam6 <- gam(data = both_sv, formula = biomass~1, method = "REML", select = T, family = tw())

AIC(biomass_gam)
AIC(biomass_gam2)
AIC(biomass_gam3)
AIC(biomass_gam4)
AIC(biomass_gam5)
AIC(biomass_gam6)





bg_fit <- add_fitted(both_sv, biomass_gam3)

ggplot(filter(bg_fit), aes(year, biomass, color = source)) + geom_point() + geom_line(aes(y = .value))

bg_samples <- gratia::fitted_samples(biomass_gam3, n = 100, seed = 1977) %>%
  group_by(row) %>%
  summarize(mean = mean(fitted),
         lower_2p5 = quantile(fitted, 0.025),
         upper_97p5  = quantile(fitted, 0.975)) %>%
  ungroup() %>%
  bind_cols(both_sv)

ggplot(filter(bg_samples), aes(year, biomass, color = source)) + geom_point() + geom_line(aes(y = mean)) + geom_ribbon(aes(ymin = lower_2p5, ymax = upper_97p5), alpha = .1)


bg_fit <- add_fitted(both_sv, biomass_gam4)

ggplot(filter(bg_fit), aes(year, biomass, color = source)) + geom_point() + geom_line(aes(y = .value))

bg_samples <- gratia::fitted_samples(biomass_gam4, n = 100, seed = 1977) %>%
  group_by(row) %>%
  summarize(mean = mean(fitted),
            lower_2p5 = quantile(fitted, 0.025),
            upper_97p5  = quantile(fitted, 0.975)) %>%
  ungroup() %>%
  bind_cols(both_sv)

ggplot(filter(bg_samples), aes(year, biomass, color = source)) + geom_point() + geom_line(aes(y = mean)) + geom_ribbon(aes(ymin = lower_2p5, ymax = upper_97p5), alpha = .1)

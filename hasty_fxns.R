library(rwar)
library(BBSsize)
library(dplyr)
library(ggplot2)
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
              total_biomass = sum(mass),
              total_energy = sum(energy)) %>%
    ungroup()%>%
    rename(timeperiod = year)

  bind_cols(sv_isd, isd$metadata$location)
}

whole_thing <- function(dat) {
  dat_sliced <-slice_dat(dat)

  dat_resp <- resp_dat(dat_sliced, resp_seed = 1989)

  dat_isd <- simulate_isd_ts(dat_sliced, isd_seed = 1989)

  dat_resp_isd <- simulate_isd_ts(dat_resp, isd_seed = 1989)



  real_sv <- calc_sv(dat_isd) %>%
    mutate(source = "real")
  sim_sv <- calc_sv(dat_resp_isd) %>%
    mutate(source = "sim")

  both_sv <- bind_rows(real_sv, sim_sv)  %>%
    mutate(fsource = as.factor(source),
           matssname = paste0("bbs_rtrg_", dat$metadata$location$route, "_", dat$metadata$location$statenum),
           simtype = "actual")

  both_sv
}

hasty_models <- function(sims) {


  models <- list(

    gaussian_glm_full = glm(total_biomass ~ timeperiod * source, data= sims),
    gaussian_glm_noint = glm(total_biomass ~ timeperiod + source, data= sims),
    gaussian_glm_nos = glm(total_biomass ~ timeperiod, data= sims),
    gaussian_glm_notime = glm(total_biomass ~ 1, data = sims),


    gamma_glm_full = glm(total_biomass ~ timeperiod * source, family = Gamma(link = "log"), data= sims),
    gamma_glm_noint = glm(total_biomass ~ timeperiod + source, family = Gamma(link = "log"), data= sims),
    gamma_glm_nos = glm(total_biomass ~ timeperiod, family = Gamma(link = "log"), data= sims),
    gamma_glm_notime = glm(total_biomass ~ 1, family = Gamma(link = "log"), data = sims)

  )

  models

}


hasty_model_aic <- function(some_models) {

  p_interaction <- anova(some_models$gaussian_glm_full, some_models$gaussian_glm_noint, test = "F")[2,6]
  p_sourceintercept <- anova(some_models$gaussian_glm_noint, some_models$gaussian_glm_nos, test = "F")[2,6]
  p_slope <- anova(some_models$gaussian_glm_nos,some_models$gaussian_glm_notime, test = "F")[2,6]

  gaussian_aics <- bind_rows(lapply(some_models[1:4], model_aic)) %>%
    mutate(model_p = c(p_interaction, p_sourceintercept, p_slope, NA),
           modelcomplexity = c(4,3,2,1))


  p_gamma_interaction <- anova(some_models$gamma_glm_full, some_models$gamma_glm_noint, test = "F")[2,6]
  p_gamma_sourceintercept <- anova(some_models$gamma_glm_noint, some_models$gamma_glm_nos, test = "F")[2,6]
  p_gamma_slope <- anova(some_models$gamma_glm_nos,some_models$gamma_glm_notime, test = "F")[2,6]


  gamma_aics <- bind_rows(lapply(some_models[5:8], model_aic)) %>%
    mutate(model_p = c(p_gamma_interaction, p_gamma_sourceintercept, p_gamma_slope, NA),
           modelcomplexity = c(4,3,2,1))

  all_aics <- bind_rows(gaussian_aics, gamma_aics)

  all_aics


}

model_aic <- function(one_model) {

  one_model_aic <- data.frame(model_AIC = AIC(one_model),
                              model_AICc = AICcmodavg::AICc(one_model)) %>%
    mutate(model_family = one_model$family$family,
           model_link = one_model$family$link,
           model_formula = toString(one_model$formula[3]),
           matssname = one_model$data$matssname[1])
}

hasty_model_predicted_change <- function(some_models) {

  predicted_changes <- lapply(some_models, model_predicted_change)

  predicted_changes <- bind_rows(predicted_changes)
  predicted_changes
}


model_predicted_change <- function(one_model) {

  model_dat <- one_model$data %>%
    mutate(predvals = predict(one_model, type = "response"))

  model_begin <- min(model_dat$timeperiod)
  model_end <- max(model_dat$timeperiod)
  model_span <- model_end - model_begin

  model_change <- model_dat %>%
    filter(timeperiod  %in% c(model_end, model_begin)) %>%
    mutate(timeperiod_name = ifelse(timeperiod == model_end, "end", "begin")) %>%
    select(timeperiod_name, predvals, source) %>%
    tidyr::pivot_wider(names_from = c(timeperiod_name, source), values_from = predvals) %>%
    mutate(ratio_sim = end_sim / begin_sim,
           ratio_real = end_real / begin_real) %>%
    mutate(model_family = one_model$family$family,
           model_link = one_model$family$link,
           model_formula = toString(one_model$formula[3]),
           model_begin = model_begin,
           model_end = model_end,
           model_span = model_span,
           matssname = model_dat$matssname[1],
           model_observations = length(unique(model_dat$timeperiod))
    )

  model_change
}

cor_compare <- function(dataset, ndraws) {


  focal_sim <- resp_dat(dataset, 1989)

  compare_sim_seeds <- (1990:(1989+ndraws))

  compare_sims <- lapply(compare_sim_seeds, resp_dat, dataset = dataset)

  focal_isd <- simulate_isd_ts(focal_sim, isd_seed = 1989)

  compare_isds <- lapply(compare_sims, simulate_isd_ts, isd_seed = 1989)

  focal_sv <- calc_sv(focal_isd)

  compare_svs <- lapply(compare_isds, calc_sv)

  cors <- unlist(lapply(compare_svs, sim_cor, focal_sv = focal_sv))

  real_isd <- simulate_isd_ts(dataset, isd_seed = 1989)

  real_sv <- calc_sv(real_isd)

  real_cor <- cor(real_sv$total_biomass, focal_sv$total_biomass)


  out <- data.frame(
    real_cor = real_cor,
    real_cor_percentile = percentile_score(real_cor, cors),
    real_cor_ses = ses(real_cor, cors),
    real_cor_diff = mean(cors) - real_cor,
    sim_cor_mean = mean(cors)
  ) %>%
    bind_cols(dataset$metadata$location) %>%
    mutate(matssname = paste0("bbs_rtrg_", route, "_", statenum))

  out
}

sim_cor <- function(compare_sv, focal_sv) {

  cor(focal_sv$total_biomass, compare_sv$total_biomass)

}

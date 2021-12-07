library(BBSsize)
library(dplyr)
library(ggplot2)

dat <- granby
ssims <- rwar::ssims_wrapper(dat, "actual",n_isd_draws = 2, ndraws=2)

ols_models <- function(ssims) {

  te_ols_full <- lm(total_energy ~ timeperiod * source, data = ssims)
  te_ols_nosource <- lm(total_energy ~ timeperiod, data = ssims)
  te_ols_notime <- lm(total_energy ~ 1, data = ssims)

  te_aics <- data.frame(
    model_name = c("te_ols_full", "te_ols_nosource", "te_ols_notime"),
    model_aic = c(AIC(te_ols_full), AIC(te_ols_nosource), AIC(te_ols_notime)),
    model_complexity = c(3, 2, 1)
  ) %>%
    mutate(best_aic = min(model_aic)) %>%
    mutate(aic_diff = model_aic - best_aic) %>%
    filter(aic_diff < 2) %>%
    mutate(lowest_complexity = min(model_complexity)) %>%
    filter(model_complexity == lowest_complexity) %>%
    mutate(currency = "energy")



  tb_ols_full <- lm(total_biomass ~ timeperiod * source, data = ssims)
  tb_ols_nosource <- lm(total_biomass ~ timeperiod, data = ssims)
  tb_ols_notime <- lm(total_biomass ~ 1, data = ssims)

  tb_aics <- data.frame(
    model_name = c("tb_ols_full", "tb_ols_nosource", "tb_ols_notime"),
    model_aic = c(AIC(tb_ols_full), AIC(tb_ols_nosource), AIC(tb_ols_notime)),
    model_complexity = c(3, 2, 1)
  ) %>%
    mutate(best_aic = min(model_aic)) %>%
    mutate(aic_diff = model_aic - best_aic) %>%
    filter(aic_diff < 2) %>%
    mutate(lowest_complexity = min(model_complexity)) %>%
    filter(model_complexity == lowest_complexity) %>%
    mutate(currency = "biomass")


}

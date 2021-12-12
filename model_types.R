
g_linear_gam_full <- gam(total_biomass ~ timeperiod * source, data= sims, method = "ML", select= T)


g_linear_gam_noint <- gam(total_biomass ~ timeperiod + source, data= sims, method = "ML", select= T)

g_linear_gam_nos <- gam(total_biomass ~ timeperiod, data= sims, method = "ML", select= T)

g_linear_gam_notime <- gam(total_biomass ~ 1, data = sims, method = "ML", select= T)


g_linear_aics <- data.frame(
  model = c('full', 'no_interaction', 'no_source', 'no_time'),
  aic = c(AIC(g_linear_gam_full), AIC(g_linear_gam_noint), AIC(g_linear_gam_nos), AIC(g_linear_gam_notime)),
  type = "g_linear"
)


g_linear_glm_full <- glm(total_biomass ~ timeperiod * source, data= sims)


g_linear_glm_noint <- glm(total_biomass ~ timeperiod + source, data= sims)

g_linear_glm_nos <- glm(total_biomass ~ timeperiod, data= sims)

g_linear_glm_notime <- glm(total_biomass ~ 1, data = sims)


glm_linear_aics <- data.frame(
  model = c('full', 'no_interaction', 'no_source', 'no_time'),
  aic = c(AIC(g_linear_glm_full), AIC(g_linear_glm_noint), AIC(g_linear_glm_nos), AIC(g_linear_glm_notime)),
  type = "g_linear"
)


g_linear_lm_full <- lm(total_biomass ~ timeperiod * source, data= sims)


g_linear_lm_noint <- lm(total_biomass ~ timeperiod + source, data= sims)

g_linear_lm_nos <- lm(total_biomass ~ timeperiod, data= sims)

g_linear_lm_notime <- lm(total_biomass ~ 1, data = sims)


lm_linear_aics <- data.frame(
  model = c('full', 'no_interaction', 'no_source', 'no_time'),
  aic = c(AIC(g_linear_lm_full), AIC(g_linear_lm_noint), AIC(g_linear_lm_nos), AIC(g_linear_lm_notime)),
  type = "g_linear"
)





g_linear_glm_gamma_full <- glm(total_biomass ~ timeperiod * source, data=sims, family = Gamma(link = "log"))


g_linear_glm_gamma_noint <- glm(total_biomass ~ timeperiod + source, data=sims, family = Gamma(link = "log"))

g_linear_glm_gamma_nos <- glm(total_biomass ~ timeperiod, data=sims, family = Gamma(link = "log"))

g_linear_glm_gamma_notime <- glm(total_biomass ~ 1, data = sims, family = Gamma(link = "log"))


glm_gamma_linear_aics <- data.frame(
  model = c('full', 'no_interaction', 'no_source', 'no_time'),
  aic = c(AIC(g_linear_glm_gamma_full), AIC(g_linear_glm_gamma_noint), AIC(g_linear_glm_gamma_nos), AIC(g_linear_glm_gamma_notime)),
  type = "g_linear"
)


library(statmod)
g_linear_glm_tweedie_full <- glm(total_biomass ~ timeperiod * source, data=sims, family = tweedie())


g_linear_glm_tweedie_noint <- glm(total_biomass ~ timeperiod + source, data=sims, family = tweedie())

g_linear_glm_tweedie_nos <- glm(total_biomass ~ timeperiod, data=sims, family = tweedie())

g_linear_glm_tweedie_notime <- glm(total_biomass ~ 1, data = sims, family = tweedie())


glm_tweedie_linear_aics <- data.frame(
  model = c('full', 'no_interaction', 'no_source', 'no_time'),
  aic = c(AIC(g_linear_glm_tweedie_full), AIC(g_linear_glm_tweedie_noint), AIC(g_linear_glm_tweedie_nos), AIC(g_linear_glm_tweedie_notime)),
  type = "g_linear"
)

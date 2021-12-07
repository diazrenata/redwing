library(dplyr)
library(ggplot2)
load(here::here("aspirational_structure", "pipelines", "portable_results_all_localn.Rds"))

all_winners %>% group_by(currency, model) %>% summarize(n = n())

actual_qis_95 <- filter(all_qis, simtype == "actual", .width == .95)

actual_qis_95 %>%
  group_by(currency) %>%
  summarize(percent_with_slope = mean(!is.na(timeperiodend)))

actual_qis_95 %>%
  group_by(currency) %>%
  summarize(percent_decreasing = mean(timeperiodend.upper < 0, na.rm = T),
            percent_increasing = mean(timeperiodend.lower > 0, na.rm = T),
            n_with_slope = sum(!is.na(timeperiodend)))

actual_qis_95 %>%
  group_by(currency) %>%
  summarize(percent_interaction = mean(!is.na(`timeperiodend:sourcecurrency`), na.rm = T))


actual_qis_95 %>%
  group_by(currency) %>%
  summarize(percent_currency_above_abund = mean((`timeperiodend:sourcecurrency.lower` >0), na.rm = T),
            percent_currency_below_abund = mean((`timeperiodend:sourcecurrency.upper` <0), na.rm = T),
            n_with_interaction = sum(!is.na(`timeperiodend:sourcecurrency`)))


How many models come out which way
================

``` r
all_winners %>%
  group_by(simtype, currency, model, matssname) %>%
  mutate(null_it = row_number()) %>%
  group_by(null_it,simtype, currency, model) %>%
  summarize(count = dplyr::n())
```

    ## `summarise()` has grouped output by 'null_it', 'simtype', 'currency'. You can override using the `.groups` argument.

<div class="kable-table">

| null\_it | simtype | currency | model                | count |
| -------: | :------ | :------- | :------------------- | ----: |
|        1 | actual  | biomass  | tb\_stanlm\_full     |   278 |
|        1 | actual  | biomass  | tb\_stanlm\_nosource |   349 |
|        1 | actual  | biomass  | tb\_stanlm\_notime   |   277 |
|        1 | actual  | energy   | te\_stanlm\_full     |   154 |
|        1 | actual  | energy   | te\_stanlm\_nosource |   410 |
|        1 | actual  | energy   | te\_stanlm\_notime   |   289 |
|        2 | actual  | biomass  | tb\_stanlm\_full     |   140 |
|        2 | actual  | biomass  | tb\_stanlm\_nosource |   198 |
|        2 | actual  | biomass  | tb\_stanlm\_notime   |   145 |
|        2 | actual  | energy   | te\_stanlm\_full     |    67 |
|        2 | actual  | energy   | te\_stanlm\_nosource |   279 |
|        2 | actual  | energy   | te\_stanlm\_notime   |   159 |
|        3 | actual  | biomass  | tb\_stanlm\_full     |    54 |
|        3 | actual  | biomass  | tb\_stanlm\_nosource |    93 |
|        3 | actual  | biomass  | tb\_stanlm\_notime   |    50 |
|        3 | actual  | energy   | te\_stanlm\_full     |    20 |
|        3 | actual  | energy   | te\_stanlm\_nosource |   148 |
|        3 | actual  | energy   | te\_stanlm\_notime   |    58 |

</div>

``` r
actual_qis <- all_qis %>% 
  group_by(currency, matssname, .width) %>%
  mutate(null_it = as.factor(row_number())) %>%
  filter(simtype == "actual") 

actual_qis_95 <- filter(actual_qis, .width == .95)
# 
# ggplot(actual_qis, aes(timeperiodend, matssname)) + geom_pointinterval(aes(xmin = timeperiodend.lower, xmax = timeperiodend.upper, width = .width)) + geom_vline(xintercept = 0) + facet_wrap(vars(null_it, currency), scales = "free")
```

Of models of actual,

``` r
actual_qis_95 %>%
  group_by(null_it,currency) %>%
  summarize(percent_with_slope = mean(!is.na(timeperiodend)))
```

    ## `summarise()` has grouped output by 'null_it'. You can override using the `.groups` argument.

<div class="kable-table">

| null\_it | currency | percent\_with\_slope |
| :------- | :------- | -------------------: |
| 1        | biomass  |            0.6723485 |
| 1        | energy   |            0.6590909 |
| 2        | biomass  |            0.7215909 |
| 2        | energy   |            0.7064394 |
| 3        | biomass  |            0.7121212 |
| 3        | energy   |            0.6761364 |

</div>

Of models with a slope…

``` r
actual_qis_95 %>%
  group_by(null_it,currency) %>%
  summarize(percent_decreasing = mean(timeperiodend.upper < 0, na.rm = T),
            percent_increasing = mean(timeperiodend.lower > 0, na.rm = T),
            n_with_slope = sum(!is.na(timeperiodend)))
```

    ## `summarise()` has grouped output by 'null_it'. You can override using the `.groups` argument.

<div class="kable-table">

| null\_it | currency | percent\_decreasing | percent\_increasing | n\_with\_slope |
| :------- | :------- | ------------------: | ------------------: | -------------: |
| 1        | biomass  |           0.5690141 |           0.2169014 |            355 |
| 1        | energy   |           0.6293103 |           0.2931034 |            348 |
| 2        | biomass  |           0.5774278 |           0.2440945 |            381 |
| 2        | energy   |           0.6407507 |           0.2761394 |            373 |
| 3        | biomass  |           0.5930851 |           0.2180851 |            376 |
| 3        | energy   |           0.6666667 |           0.2549020 |            357 |

</div>

using a 95% CI above or below 0. This can sum to less than 1 if there is
a model with an interaction in which the abundance-slope is over 0, but
the currency offset is nonzero.

Of all models, here’s the proportion with an interaction:

``` r
actual_qis_95 %>%
  group_by(null_it,currency) %>%
  summarize(percent_interaction = mean(!is.na(`timeperiodend:sourcecurrency`), na.rm = T))
```

    ## `summarise()` has grouped output by 'null_it'. You can override using the `.groups` argument.

<div class="kable-table">

| null\_it | currency | percent\_interaction |
| :------- | :------- | -------------------: |
| 1        | biomass  |            0.2803030 |
| 1        | energy   |            0.1287879 |
| 2        | biomass  |            0.3030303 |
| 2        | energy   |            0.1609848 |
| 3        | biomass  |            0.3106061 |
| 3        | energy   |            0.1666667 |

</div>

Of models with an interaction…

``` r
actual_qis_95 %>%
  group_by(null_it,currency) %>%
  summarize(percent_currency_above_abund = mean((`timeperiodend:sourcecurrency.lower` >0), na.rm = T),
            percent_currency_below_abund = mean((`timeperiodend:sourcecurrency.upper` <0), na.rm = T),
            n_with_interaction = sum(!is.na(`timeperiodend:sourcecurrency`)))
```

    ## `summarise()` has grouped output by 'null_it'. You can override using the `.groups` argument.

<div class="kable-table">

| null\_it | currency | percent\_currency\_above\_abund | percent\_currency\_below\_abund | n\_with\_interaction |
| :------- | :------- | ------------------------------: | ------------------------------: | -------------------: |
| 1        | biomass  |                       0.7905405 |                       0.1891892 |                  148 |
| 1        | energy   |                       0.6911765 |                       0.2941176 |                   68 |
| 2        | biomass  |                       0.7000000 |                       0.2750000 |                  160 |
| 2        | energy   |                       0.6352941 |                       0.3647059 |                   85 |
| 3        | biomass  |                       0.7865854 |                       0.1951220 |                  164 |
| 3        | energy   |                       0.7727273 |                       0.2159091 |                   88 |

</div>

``` r
# 
# ggplot(actual_qis, aes(`timeperiodend:sourcecurrency`, matssname)) + geom_pointinterval(aes(xmin = `timeperiodend:sourcecurrency.lower`, xmax = `timeperiodend:sourcecurrency.upper`, width = .width)) + geom_vline(xintercept = 0) + facet_wrap(vars(currency), scales = "free")
```

``` r
raw_changes <- all_sims %>%
  filter(simtype == "actual") %>%
  group_by(matssname, source, timeperiod) %>%
  summarize(total_energy = mean(total_energy),
            total_biomass = mean(total_biomass)) %>%
  ungroup() %>%
  tidyr::pivot_wider(id_cols = c(matssname, source), names_from = timeperiod, values_from = c(total_energy, total_biomass)) %>%
  mutate(energy_ratio = total_energy_end / total_energy_begin,
         biomass_ratio = total_biomass_end / total_biomass_begin) %>%
  select(matssname, source, energy_ratio, biomass_ratio) %>%
  tidyr::pivot_wider(id_cols = matssname, names_from = source, values_from = c(energy_ratio, biomass_ratio))
```

    ## `summarise()` has grouped output by 'matssname', 'source'. You can override using the `.groups` argument.

``` r
sig_changes <- raw_changes %>%
  left_join(actual_qis_95) %>%
  mutate(abund_slope_nonzero = (timeperiodend.upper < 0 | timeperiodend.lower > 0) & !is.na(timeperiodend),
         currency_offset_nonzero =(`timeperiodend:sourcecurrency.upper` < 0 | `timeperiodend:sourcecurrency.lower` > 0) & !is.na(`timeperiodend:sourcecurrency`))
```

    ## Joining, by = "matssname"

``` r
#  
# ggplot(sig_changes, aes(energy_ratio_abundance, energy_ratio_currency)) + geom_point(alpha = .1) + geom_point(data =filter(sig_changes, abund_slope_nonzero), aes(color = currency_offset_nonzero), alpha = 1) + geom_abline(slope= 1, intercept = 0) + geom_vline(xintercept = 1) + geom_hline(yintercept = 1) + theme(legend.position = "bottom") + ggtitle("Energy")
# 
# 
# ggplot(sig_changes, aes(biomass_ratio_abundance, biomass_ratio_currency)) + geom_point(alpha = .1) + geom_point(data =filter(sig_changes, !is.na(timeperiodend)), aes(color = currency_offset_nonzero), alpha = 1) + geom_abline(slope= 1, intercept = 0) + geom_vline(xintercept = 1) + geom_hline(yintercept = 1) + theme(legend.position = "bottom") + ggtitle("Biomass")
```

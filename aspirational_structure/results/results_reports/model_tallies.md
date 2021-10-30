How many models come out which way
================

``` r
all_winners %>%
  group_by(simtype, currency, model) %>%
  summarize(count = dplyr::n())
```

    ## `summarise()` has grouped output by 'simtype', 'currency'. You can override using the `.groups` argument.

<div class="kable-table">

| simtype      | currency | model             | count |
| :----------- | :------- | :---------------- | ----: |
| actual       | biomass  | tb\_brm\_full     |    33 |
| actual       | biomass  | tb\_brm\_nosource |    36 |
| actual       | biomass  | tb\_brm\_notime   |    32 |
| actual       | energy   | te\_brm\_full     |    18 |
| actual       | energy   | te\_brm\_nosource |    49 |
| actual       | energy   | te\_brm\_notime   |    34 |
| nochange     | biomass  | tb\_brm\_notime   |   101 |
| nochange     | energy   | te\_brm\_notime   |   101 |
| nosizechange | biomass  | tb\_brm\_nosource |    67 |
| nosizechange | biomass  | tb\_brm\_notime   |    34 |
| nosizechange | energy   | te\_brm\_nosource |    68 |
| nosizechange | energy   | te\_brm\_notime   |    33 |

</div>

``` r
actual_qis <- all_qis %>% 
  filter(simtype == "actual") 
actual_qis_95 <- filter(actual_qis, .width == .95)

ggplot(actual_qis, aes(timeperiodend, matssname)) + geom_pointinterval(aes(xmin = timeperiodend.lower, xmax = timeperiodend.upper, width = .width)) + geom_vline(xintercept = 0) + facet_wrap(vars(currency), scales = "free")
```

    ## Warning: Removed 64 rows containing missing values (geom_segment).

    ## Warning: Removed 68 rows containing missing values (geom_segment).

![](model_tallies_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

Of models of actual,

``` r
actual_qis_95 %>%
  group_by(currency) %>%
  summarize(percent_with_slope = mean(!is.na(timeperiodend)))
```

<div class="kable-table">

| currency | percent\_with\_slope |
| :------- | -------------------: |
| biomass  |            0.6831683 |
| energy   |            0.6633663 |

</div>

Of models with a slope…

``` r
actual_qis_95 %>%
  group_by(currency) %>%
  summarize(percent_decreasing = mean(timeperiodend.upper < 0, na.rm = T),
            percent_increasing = mean(timeperiodend.lower > 0, na.rm = T),
            n_with_slope = sum(!is.na(timeperiodend)))
```

<div class="kable-table">

| currency | percent\_decreasing | percent\_increasing | n\_with\_slope |
| :------- | ------------------: | ------------------: | -------------: |
| biomass  |           0.6086957 |           0.2318841 |             69 |
| energy   |           0.7014925 |           0.2388060 |             67 |

</div>

using a 95% CI above or below 0. This can sum to less than 1 if there is
a model with an interaction in which the abundance-slope is over 0, but
the currency offset is nonzero.

Of all models, here’s the proportion with an interaction:

``` r
actual_qis_95 %>%
  group_by(currency) %>%
  summarize(percent_interaction = mean(!is.na(`timeperiodend:sourcecurrency`), na.rm = T))
```

<div class="kable-table">

| currency | percent\_interaction |
| :------- | -------------------: |
| biomass  |            0.3267327 |
| energy   |            0.1782178 |

</div>

Of models with an interaction…

``` r
actual_qis_95 %>%
  group_by(currency) %>%
  summarize(percent_currency_above_abund = mean((`timeperiodend:sourcecurrency.lower` >0), na.rm = T),
            percent_currency_below_abund = mean((`timeperiodend:sourcecurrency.upper` <0), na.rm = T),
            n_with_interaction = sum(!is.na(`timeperiodend:sourcecurrency`)))
```

<div class="kable-table">

| currency | percent\_currency\_above\_abund | percent\_currency\_below\_abund | n\_with\_interaction |
| :------- | ------------------------------: | ------------------------------: | -------------------: |
| biomass  |                       0.8181818 |                       0.1818182 |                   33 |
| energy   |                       0.6666667 |                       0.3333333 |                   18 |

</div>

``` r
ggplot(actual_qis, aes(`timeperiodend:sourcecurrency`, matssname)) + geom_pointinterval(aes(xmin = `timeperiodend:sourcecurrency.lower`, xmax = `timeperiodend:sourcecurrency.upper`, width = .width)) + geom_vline(xintercept = 0) + facet_wrap(vars(currency), scales = "free")
```

    ## Warning: Removed 136 rows containing missing values (geom_segment).

    ## Warning: Removed 166 rows containing missing values (geom_segment).

![](model_tallies_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

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
ggplot(sig_changes, aes(energy_ratio_abundance, energy_ratio_currency)) + geom_point(alpha = .1) + geom_point(data =filter(sig_changes, abund_slope_nonzero), aes(color = currency_offset_nonzero), alpha = 1) + geom_abline(slope= 1, intercept = 0) + geom_vline(xintercept = 1) + geom_hline(yintercept = 1) + theme(legend.position = "bottom") + ggtitle("Energy")
```

![](model_tallies_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
ggplot(sig_changes, aes(biomass_ratio_abundance, biomass_ratio_currency)) + geom_point(alpha = .1) + geom_point(data =filter(sig_changes, !is.na(timeperiodend)), aes(color = currency_offset_nonzero), alpha = 1) + geom_abline(slope= 1, intercept = 0) + geom_vline(xintercept = 1) + geom_hline(yintercept = 1) + theme(legend.position = "bottom") + ggtitle("Biomass")
```

![](model_tallies_files/figure-gfm/unnamed-chunk-7-2.png)<!-- -->

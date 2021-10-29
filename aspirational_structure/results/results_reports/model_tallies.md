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
| actual       | biomass  | tb\_brm\_full     |    11 |
| actual       | biomass  | tb\_brm\_nosource |     6 |
| actual       | biomass  | tb\_brm\_notime   |     7 |
| actual       | energy   | te\_brm\_full     |     7 |
| actual       | energy   | te\_brm\_nosource |     9 |
| actual       | energy   | te\_brm\_notime   |     8 |
| nochange     | biomass  | tb\_brm\_notime   |    24 |
| nochange     | energy   | te\_brm\_notime   |    24 |
| nosizechange | biomass  | tb\_brm\_nosource |    15 |
| nosizechange | biomass  | tb\_brm\_notime   |     9 |
| nosizechange | energy   | te\_brm\_nosource |    15 |
| nosizechange | energy   | te\_brm\_notime   |     9 |

</div>

From 24 sites…

  - 7 (for biomass) and 8 (for energy) models don’t pick up on any
    difference begin-end
  - 11 (for biomass) and 7 (for energy) models pick up on *different*
    dynamics for the currency than if the size structure were more or
    less replicated.
  - Biomass picks up on an interaction more often than energy.
  - These tallies don’t tell us how often the abundance slope is nonzero
    in the full models

<!-- end list -->

``` r
actual_qis <- all_qis %>% 
  filter(simtype == "actual") 
actual_qis_95 <- filter(actual_qis, .width == .95)

ggplot(actual_qis, aes(b_timeperiodend, matssname)) + geom_pointinterval(aes(xmin = b_timeperiodend.lower, xmax = b_timeperiodend.upper, width = .width)) + geom_vline(xintercept = 0) + facet_wrap(vars(currency), scales = "free")
```

    ## Warning: Removed 14 rows containing missing values (geom_segment).

    ## Warning: Removed 16 rows containing missing values (geom_segment).

![](model_tallies_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

Of models of actual,

``` r
actual_qis_95 %>%
  group_by(currency) %>%
  summarize(percent_with_slope = mean(!is.na(b_timeperiodend)))
```

<div class="kable-table">

| currency | percent\_with\_slope |
| :------- | -------------------: |
| biomass  |            0.7083333 |
| energy   |            0.6666667 |

</div>

Of models with a slope…

``` r
actual_qis_95 %>%
  group_by(currency) %>%
  summarize(percent_decreasing = mean(b_timeperiodend.upper < 0, na.rm = T),
            percent_increasing = mean(b_timeperiodend.lower > 0, na.rm = T))
```

<div class="kable-table">

| currency | percent\_decreasing | percent\_increasing |
| :------- | ------------------: | ------------------: |
| biomass  |           0.5882353 |           0.2941176 |
| energy   |           0.6250000 |           0.3125000 |

</div>

using a 95% CI above or below 0. This can sum to less than 1 if there is
a model with an interaction in which the abundance-slope is over 0, but
the currency offset is nonzero.

Of all models, here’s the proportion with an interaction:

``` r
actual_qis_95 %>%
  group_by(currency) %>%
  summarize(percent_interaction = mean(!is.na(`b_timeperiodend:sourcecurrency`), na.rm = T))
```

<div class="kable-table">

| currency | percent\_interaction |
| :------- | -------------------: |
| biomass  |            0.4583333 |
| energy   |            0.2916667 |

</div>

Of models with an interaction…

``` r
actual_qis_95 %>%
  group_by(currency) %>%
  summarize(percent_currency_above_abund = mean((`b_timeperiodend:sourcecurrency.lower` >0), na.rm = T),
            percent_currency_below_abund = mean((`b_timeperiodend:sourcecurrency.upper` <0), na.rm = T))
```

<div class="kable-table">

| currency | percent\_currency\_above\_abund | percent\_currency\_below\_abund |
| :------- | ------------------------------: | ------------------------------: |
| biomass  |                       0.7272727 |                       0.2727273 |
| energy   |                       0.5714286 |                       0.4285714 |

</div>

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
  mutate(abund_slope_nonzero = (b_timeperiodend.upper < 0 | b_timeperiodend.lower > 0) & !is.na(b_timeperiodend),
         currency_offset_nonzero =(`b_timeperiodend:sourcecurrency.upper` < 0 | `b_timeperiodend:sourcecurrency.lower` > 0) & !is.na(`b_timeperiodend:sourcecurrency`))
```

    ## Joining, by = "matssname"

``` r
ggplot(sig_changes, aes(energy_ratio_abundance, energy_ratio_currency)) + geom_point(alpha = .1) + geom_point(data =filter(sig_changes, abund_slope_nonzero), aes(color = currency_offset_nonzero), alpha = 1) + geom_abline(slope= 1, intercept = 0) + geom_vline(xintercept = 1) + geom_hline(yintercept = 1) + theme(legend.position = "bottom") + ggtitle("Energy")
```

![](model_tallies_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
ggplot(sig_changes, aes(biomass_ratio_abundance, biomass_ratio_currency)) + geom_point(alpha = .1) + geom_point(data =filter(sig_changes, !is.na(b_timeperiodend)), aes(color = currency_offset_nonzero), alpha = 1) + geom_abline(slope= 1, intercept = 0) + geom_vline(xintercept = 1) + geom_hline(yintercept = 1) + theme(legend.position = "bottom") + ggtitle("Biomass")
```

![](model_tallies_files/figure-gfm/unnamed-chunk-7-2.png)<!-- -->

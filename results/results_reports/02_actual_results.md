How many models come out which way
================

``` r
all_winners %>%
  group_by(simtype, currency, model) %>%
  summarize(count = dplyr::n()) 
```

    ## `summarise()` has grouped output by 'simtype', 'currency'. You can override using the `.groups` argument.

<div class="kable-table">

| simtype      | currency | model                | count |
| :----------- | :------- | :------------------- | ----: |
| actual       | biomass  | tb\_stanlm\_full     |   134 |
| actual       | biomass  | tb\_stanlm\_nosource |   218 |
| actual       | biomass  | tb\_stanlm\_notime   |   176 |
| actual       | energy   | te\_stanlm\_full     |    71 |
| actual       | energy   | te\_stanlm\_nosource |   292 |
| actual       | energy   | te\_stanlm\_notime   |   165 |
| nochange     | biomass  | tb\_stanlm\_nosource |     4 |
| nochange     | biomass  | tb\_stanlm\_notime   |   524 |
| nochange     | energy   | te\_stanlm\_nosource |     1 |
| nochange     | energy   | te\_stanlm\_notime   |   527 |
| nosizechange | biomass  | tb\_stanlm\_nosource |   323 |
| nosizechange | biomass  | tb\_stanlm\_notime   |   205 |
| nosizechange | energy   | te\_stanlm\_nosource |   348 |
| nosizechange | energy   | te\_stanlm\_notime   |   180 |

</div>

``` r
actual_qis <- all_qis %>% 
  filter(simtype == "actual") 
actual_qis_95 <- filter(actual_qis, .width == .95)

ggplot(actual_qis, aes(timeperiodend, matssname)) + geom_pointinterval(aes(xmin = timeperiodend.lower, xmax = timeperiodend.upper, width = .width)) + geom_vline(xintercept = 0) + facet_wrap(vars(currency), scales = "free")
```

    ## Warning: Removed 352 rows containing missing values (geom_segment).

    ## Warning: Removed 330 rows containing missing values (geom_segment).

![](02_actual_results_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

Of models of actual,

``` r
actual_qis_95 %>%
  group_by(currency) %>%
  summarize(percent_with_slope = mean(!is.na(timeperiodend)))
```

<div class="kable-table">

| currency | percent\_with\_slope |
| :------- | -------------------: |
| biomass  |            0.6666667 |
| energy   |            0.6875000 |

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
| biomass  |           0.6022727 |           0.2159091 |            352 |
| energy   |           0.6804408 |           0.2506887 |            363 |

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
| biomass  |            0.2537879 |
| energy   |            0.1344697 |

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
| biomass  |                       0.7388060 |                       0.2313433 |                  134 |
| energy   |                       0.5915493 |                       0.3098592 |                   71 |

</div>

``` r
ggplot(actual_qis, aes(`timeperiodend:sourcecurrency`, matssname)) + geom_pointinterval(aes(xmin = `timeperiodend:sourcecurrency.lower`, xmax = `timeperiodend:sourcecurrency.upper`, width = .width)) + geom_vline(xintercept = 0) + facet_wrap(vars(currency), scales = "free")
```

    ## Warning: Removed 788 rows containing missing values (geom_segment).

    ## Warning: Removed 914 rows containing missing values (geom_segment).

![](02_actual_results_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

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

![](02_actual_results_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
ggplot(sig_changes, aes(biomass_ratio_abundance, biomass_ratio_currency)) + geom_point(alpha = .1) + geom_point(data =filter(sig_changes, !is.na(timeperiodend)), aes(color = currency_offset_nonzero), alpha = 1) + geom_abline(slope= 1, intercept = 0) + geom_vline(xintercept = 1) + geom_hline(yintercept = 1) + theme(legend.position = "bottom") + ggtitle("Biomass")
```

![](02_actual_results_files/figure-gfm/unnamed-chunk-7-2.png)<!-- -->

## Filtered to max 10

Here is the above, but with a maximum of 10 routes per BCR (n = 238
overall)

``` r
all_winners_f <- filter(all_winners, matssname %in% working_datasets$matssname)
all_qis_f <- filter(all_qis, matssname %in% working_datasets$matssnam)
all_winners_f %>%
  group_by(simtype, currency, model) %>%
  summarize(count = dplyr::n()) 
```

    ## `summarise()` has grouped output by 'simtype', 'currency'. You can override using the `.groups` argument.

<div class="kable-table">

| simtype      | currency | model                | count |
| :----------- | :------- | :------------------- | ----: |
| actual       | biomass  | tb\_stanlm\_full     |    57 |
| actual       | biomass  | tb\_stanlm\_nosource |    93 |
| actual       | biomass  | tb\_stanlm\_notime   |    88 |
| actual       | energy   | te\_stanlm\_full     |    33 |
| actual       | energy   | te\_stanlm\_nosource |   128 |
| actual       | energy   | te\_stanlm\_notime   |    77 |
| nochange     | biomass  | tb\_stanlm\_nosource |     1 |
| nochange     | biomass  | tb\_stanlm\_notime   |   237 |
| nochange     | energy   | te\_stanlm\_nosource |     1 |
| nochange     | energy   | te\_stanlm\_notime   |   237 |
| nosizechange | biomass  | tb\_stanlm\_nosource |   144 |
| nosizechange | biomass  | tb\_stanlm\_notime   |    94 |
| nosizechange | energy   | te\_stanlm\_nosource |   158 |
| nosizechange | energy   | te\_stanlm\_notime   |    80 |

</div>

``` r
actual_qis_f <- all_qis_f %>% 
  filter(simtype == "actual") 
actual_qis_95_f <- filter(actual_qis_f, .width == .95)

ggplot(actual_qis_f, aes(timeperiodend, matssname)) + geom_pointinterval(aes(xmin = timeperiodend.lower, xmax = timeperiodend.upper, width = .width)) + geom_vline(xintercept = 0) + facet_wrap(vars(currency), scales = "free")
```

    ## Warning: Removed 176 rows containing missing values (geom_segment).

    ## Warning: Removed 154 rows containing missing values (geom_segment).

![](02_actual_results_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

Of models of actual,

``` r
actual_qis_95_f %>%
  group_by(currency) %>%
  summarize(percent_with_slope = mean(!is.na(timeperiodend)))
```

<div class="kable-table">

| currency | percent\_with\_slope |
| :------- | -------------------: |
| biomass  |            0.6302521 |
| energy   |            0.6764706 |

</div>

Of models with a slope…

``` r
actual_qis_95_f %>%
  group_by(currency) %>%
  summarize(percent_decreasing = mean(timeperiodend.upper < 0, na.rm = T),
            percent_increasing = mean(timeperiodend.lower > 0, na.rm = T),
            n_with_slope = sum(!is.na(timeperiodend)))
```

<div class="kable-table">

| currency | percent\_decreasing | percent\_increasing | n\_with\_slope |
| :------- | ------------------: | ------------------: | -------------: |
| biomass  |           0.5933333 |           0.2200000 |            150 |
| energy   |           0.6708075 |           0.2484472 |            161 |

</div>

using a 95% CI above or below 0. This can sum to less than 1 if there is
a model with an interaction in which the abundance-slope is over 0, but
the currency offset is nonzero.

Of all models, here’s the proportion with an interaction:

``` r
actual_qis_95_f %>%
  group_by(currency) %>%
  summarize(percent_interaction = mean(!is.na(`timeperiodend:sourcecurrency`), na.rm = T))
```

<div class="kable-table">

| currency | percent\_interaction |
| :------- | -------------------: |
| biomass  |            0.2394958 |
| energy   |            0.1386555 |

</div>

Of models with an interaction…

``` r
actual_qis_95_f %>%
  group_by(currency) %>%
  summarize(percent_currency_above_abund = mean((`timeperiodend:sourcecurrency.lower` >0), na.rm = T),
            percent_currency_below_abund = mean((`timeperiodend:sourcecurrency.upper` <0), na.rm = T),
            n_with_interaction = sum(!is.na(`timeperiodend:sourcecurrency`)))
```

<div class="kable-table">

| currency | percent\_currency\_above\_abund | percent\_currency\_below\_abund | n\_with\_interaction |
| :------- | ------------------------------: | ------------------------------: | -------------------: |
| biomass  |                       0.7543860 |                       0.1929825 |                   57 |
| energy   |                       0.6666667 |                       0.2424242 |                   33 |

</div>

``` r
ggplot(actual_qis_f, aes(`timeperiodend:sourcecurrency`, matssname)) + geom_pointinterval(aes(xmin = `timeperiodend:sourcecurrency.lower`, xmax = `timeperiodend:sourcecurrency.upper`, width = .width)) + geom_vline(xintercept = 0) + facet_wrap(vars(currency), scales = "free")
```

    ## Warning: Removed 362 rows containing missing values (geom_segment).

    ## Warning: Removed 410 rows containing missing values (geom_segment).

![](02_actual_results_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->
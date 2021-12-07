Actual results
================
Renata Diaz
2021-10-02

This whole approach bugs me because of the sheer number of comparisons
being made and the .05 threshold. So, don’t overinterpret in any single
instance.

``` r
ggplot(all_results, aes(overall_p)) +geom_histogram()
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](interaction_results_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

``` r
all_results <- all_results %>%
  group_by_all() %>%
  mutate(any_terms_sig = all(overall_p < .05, any(
    `Pr(>|t|)_timeperiodend:currencybiomass` < .05,
    `Pr(>|t|)_timeperiodend:currencyenergy` < .05,
    `Pr(>|t|)_timeperiodend` < .05
  )),
  any_contrasts_sig = any(
    abundance_contrastp.value < .05,
    energy_contrastp.value < .05,
    biomass_contrastp.value < .05
  ),
  abundance_up = abundance_raw_ratio >=1,
  biomass_up = biomass_raw_ratio >=1,
  energy_up = energy_raw_ratio >=1) %>%
  ungroup()
```

``` r
mean(all_results$overall_sig)
```

    ## [1] 0.5662879

``` r
mean(all_results$overall_sig_rs)
```

    ## [1] 0.6231061

``` r
mean(all_results$change_sig)
```

    ## [1] 0.5340909

``` r
mean(all_results$change_sig_rs)
```

    ## [1] 0.5757576

``` r
mean(filter(all_results, change_sig)$currency_slopes_different)
```

    ## [1] 0.1453901

``` r
mean(filter(all_results, change_sig_rs)$currency_slopes_different_rs)
```

    ## [1] 0.2368421

So 0.5340909 percent of the time we have both the overall lm significant
and at least one **slope** term is significant. (There are 25 (in the
full dataset/16 in the max-10 subset) routes where the overall model is
significant but it’s either the intercept or one of the currency
intercepts).

``` r
sig_model <- all_results %>% 
  filter(change_sig)%>%
  group_by_all() %>%
  mutate(all_together = all(
    `Pr(>|t|)_timeperiodend:currencybiomass` > .05,
    `Pr(>|t|)_timeperiodend:currencyenergy` > .05,
    `Pr(>|t|)_timeperiodend` < .05
  )) %>%
  ungroup()
```

``` r
mean(sig_model$all_together)
```

    ## [1] 0.8546099

Of the models that are significant overall and have a significant slope,
0.8546099 of them have a significant time slope but not a significant
interaction of time x currency. Meaning that biomass, energy use, and
abundance have all *changed* but have not changed *differently from each
other*.

When they **are** all changing, but changing together:

``` r
all_together <- filter(sig_model, all_together)

ggplot(all_together, aes(abundance_raw_ratio)) + geom_histogram()
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](interaction_results_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
ggplot(all_together, aes(biomass_raw_ratio)) + geom_histogram()
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](interaction_results_files/figure-gfm/unnamed-chunk-5-2.png)<!-- -->

``` r
ggplot(all_together, aes(energy_raw_ratio)) + geom_histogram()
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](interaction_results_files/figure-gfm/unnamed-chunk-5-3.png)<!-- -->

``` r
summary(all_together$abundance_raw_ratio)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  0.1645  0.5661  0.7031  0.8970  1.2828  2.5352

``` r
summary(all_together$energy_raw_ratio)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  0.2004  0.5754  0.7162  0.9408  1.2856  3.8107

``` r
summary(all_together$biomass_raw_ratio)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  0.2383  0.6306  0.7762  1.0526  1.3627  4.6937

``` r
mean(all_together$abundance_up)
```

    ## [1] 0.3319502

It’s usually a decrease - 65/35 decrease/increase. The median is for end
to be 75% of begin.

When they **aren’t** changing all together…

``` r
not_together <- filter(sig_model, !all_together)

not_together %>%
  group_by(abundance_up) %>%
  summarize(e_up = sum(energy_up),
            b_up = sum(biomass_up),
            totaln = dplyr::n()) %>%
  ungroup()
```

<div class="kable-table">

| abundance\_up | e\_up | b\_up | totaln |
| :------------ | ----: | ----: | -----: |
| FALSE         |    20 |    32 |     34 |
| TRUE          |     3 |     2 |      7 |

</div>

When abundance increases, energy decreases 6/13 times. When abundance
decreases, energy increases 25/59 times. So that 31/72 incidences of a
change in direction for *energy*.

When abundance increases, biomass decreases 8/13 times. When abundance
decreases, biomass increases 44/59 times. So that 52/72 incidences of a
change in direction for *biomass*.

If it’s not a change in direction, it’s an intensification/dampening of
the *slope* of change.

``` r
ggplot(sig_model, aes(abundance_raw_ratio, energy_raw_ratio, color = all_together)) + geom_point() + onetoone
```

![](interaction_results_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
ggplot(sig_model, aes(abundance_raw_ratio, biomass_raw_ratio, color = all_together)) + geom_point() + onetoone
```

![](interaction_results_files/figure-gfm/unnamed-chunk-7-2.png)<!-- -->

``` r
ggplot(sig_model, aes(isd_turnover)) + geom_histogram()
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](interaction_results_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
ggplot(sig_model, aes(isd_turnover)) + geom_histogram() + facet_wrap(vars(all_together))
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](interaction_results_files/figure-gfm/unnamed-chunk-8-2.png)<!-- -->

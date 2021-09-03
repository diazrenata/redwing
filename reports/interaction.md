Currency x time interactions
================
Renata Diaz
2021-09-03

I **think** this may be a more valid way to get at the thing that bugs
me.

So we are basically asking if the slope of energy change is
proportionate to abundance, if the different currencies matter or not.

I’ve been trying to get at this via a roundabout way of individual lms
on the different currencies.

I think a more succinct and probably correct - given the way I interpret
this stuff - way is to fit a lm() (or whatever, glm, nlme) of the
structure `abundance ~ timeperiod * currency`. This asks whether:

  - abundance is different in different time periods
  - the currencies have different intercepts
  - and if the currencies differ in how different they are across
    different time periods.

The currencies are on vastly different scales, right; abundance is going
to be in the hundreds while energy is in the hundreds of thousands. I
tried it using them notscaled and it didn’t work at all; any trend in
abundance (or absence) would be swamped by the variable on the larger
scale.

So here I am rescaling the variables and then putting them all in a lm
of the above form, and then looking at the different trends for each
currency and whether the currencies get different slopes.

I **believe** this gets at “decoupling” more succinctly than a million
lms and then histograms.

And then in aggregate, looking at:

  - how often biomass\!=abundance, how often energy \!= abundance
  - what’s going on with the isd in these scenarios

<!-- end list -->

``` r
weird <- filter(all_results, abs(1 - fitted_ratio_caps_energy) < .1, abs(1 - fitted_ratio_caps_abundance) > .3)

## Set up the cache and config
db <- DBI::dbConnect(RSQLite::SQLite(), here::here("drake-cache-actual.sqlite"))
cache <- storr::storr_dbi("datatable", "keystable", db)
cache$del(key = "lock", namespace = "session")

loadd(bbs_rtrg_153_14, cache = cache)

gh <- bbs_rtrg_153_14

library(BBSsize)

gh_isd <- simulate_isd_ts(gh)
```

    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"

``` r
library(rwar)

gh_sv <- get_annual_svs(gh_isd$isd) %>%
  filter(year %in% c(1988:2018))

ggplot(gh_sv, aes(year, abundance)) + geom_line() 
```

![](interaction_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

``` r
ggplot(gh_sv, aes(year, energy)) + geom_line() 
```

![](interaction_files/figure-gfm/unnamed-chunk-1-2.png)<!-- -->

``` r
gh_caps <- pull_caps(gh_sv, c(1988:1992), c(2014:2018))

gh_caps_long <- gh_caps %>%
  mutate(energy = scale((energy)),
        abundance = scale((abundance)),
        biomass = scale(biomass)) %>%
  tidyr::pivot_longer(c(-year, -timeperiod), names_to = "currency", values_to = "val")

gh_caps_forlm <- gh_caps_long %>%
  filter(currency %in% c("energy", "abundance", "biomass"))

ggplot(gh_caps_long, aes(timeperiod, val, color = currency)) + geom_point()+ facet_wrap(vars(currency), scales = "free_y")
```

![](interaction_files/figure-gfm/unnamed-chunk-1-3.png)<!-- -->

``` r
gh_lm <- lm(val ~ timeperiod * currency, data = gh_caps_forlm)

summary(gh_lm)
```

    ## 
    ## Call:
    ## lm(formula = val ~ timeperiod * currency, data = gh_caps_forlm)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1.36139 -0.41896 -0.03209  0.46273  1.72741 
    ## 
    ## Coefficients:
    ##                               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                     0.8437     0.3335   2.529  0.01841 *  
    ## timeperiodend                  -1.6873     0.4717  -3.577  0.00152 ** 
    ## currencybiomass                -1.6084     0.4717  -3.410  0.00230 ** 
    ## currencyenergy                 -0.5820     0.4717  -1.234  0.22918    
    ## timeperiodend:currencybiomass   3.2167     0.6671   4.822 6.53e-05 ***
    ## timeperiodend:currencyenergy    1.1641     0.6671   1.745  0.09377 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.7458 on 24 degrees of freedom
    ## Multiple R-squared:  0.5055, Adjusted R-squared:  0.4025 
    ## F-statistic: 4.908 on 5 and 24 DF,  p-value: 0.003109

``` r
gh_lm_fit <- gh_caps_forlm %>%
  mutate(lm_fit = gh_lm$fitted.values)

ggplot(gh_lm_fit, aes(year, val, color = currency)) + geom_point() + geom_point(aes(y = lm_fit), shape = 3) + facet_wrap(vars(currency))
```

![](interaction_files/figure-gfm/unnamed-chunk-1-4.png)<!-- -->

``` r
sp <- readd(bbs_rtrg_37_46, cache = cache)


sp_isd <- simulate_isd_ts(sp)
```

    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"

``` r
sp_sv <- get_annual_svs(sp_isd$isd) %>%
  filter(year %in% c(1988:2018))

ggplot(sp_sv, aes(year, abundance)) + geom_line() 
```

![](interaction_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
ggplot(sp_sv, aes(year, energy)) + geom_line() 
```

![](interaction_files/figure-gfm/unnamed-chunk-2-2.png)<!-- -->

``` r
sp_caps <- pull_caps(sp_sv, c(1988:1992), c(2014:2018))

sp_caps_long <- sp_caps %>%
  mutate(energy = scale((energy)),
        abundance = scale((abundance)),
        biomass = scale(biomass)) %>%
  tidyr::pivot_longer(c(-year, -timeperiod), names_to = "currency", values_to = "val")

sp_caps_forlm <- sp_caps_long %>%
  filter(currency %in% c("energy", "abundance", "biomass"))

ggplot(sp_caps_long, aes(timeperiod, val, color = currency)) + geom_point()+ facet_wrap(vars(currency), scales = "free_y")
```

![](interaction_files/figure-gfm/unnamed-chunk-2-3.png)<!-- -->

``` r
sp_lm <- lm(val ~ timeperiod * currency, data = sp_caps_forlm)

summary(sp_lm)
```

    ## 
    ## Call:
    ## lm(formula = val ~ timeperiod * currency, data = sp_caps_forlm)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1.44934 -0.27276 -0.09507  0.40628  1.26633 
    ## 
    ## Coefficients:
    ##                               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                     0.4339     0.3155   1.375 0.181734    
    ## timeperiodend                  -0.8678     0.4462  -1.945 0.063582 .  
    ## currencybiomass                -1.2719     0.4462  -2.850 0.008827 ** 
    ## currencyenergy                 -1.2182     0.4462  -2.730 0.011668 *  
    ## timeperiodend:currencybiomass   2.5437     0.6310   4.031 0.000487 ***
    ## timeperiodend:currencyenergy    2.4364     0.6310   3.861 0.000748 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.7055 on 24 degrees of freedom
    ## Multiple R-squared:  0.5576, Adjusted R-squared:  0.4654 
    ## F-statistic:  6.05 on 5 and 24 DF,  p-value: 0.0009287

``` r
sp_lm_fit <- sp_caps_forlm %>%
  mutate(lm_fit = sp_lm$fitted.values)

ggplot(sp_lm_fit, aes(year, val, color = currency)) + geom_point() + geom_point(aes(y = lm_fit), shape = 3) + facet_wrap(vars(currency))
```

![](interaction_files/figure-gfm/unnamed-chunk-2-4.png)<!-- -->

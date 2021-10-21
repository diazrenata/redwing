Change over time in different currencies
================

  - [One route](#one-route)
      - [Pull and examine raw data](#pull-and-examine-raw-data)
      - [Test for size-structure-driven
        decoupling](#test-for-size-structure-driven-decoupling)
          - [Arguments and explanation](#arguments-and-explanation)
          - [Construct sampling gmms](#construct-sampling-gmms)
          - [Get template ISD dataframes](#get-template-isd-dataframes)
          - [Draw individuals corresponding to “sim” (no size change)
            and actual
            dynamics](#draw-individuals-corresponding-to-sim-no-size-change-and-actual-dynamics)
          - [Combine with “raw”](#combine-with-raw)
          - [Compute time-period level state
            variables](#compute-time-period-level-state-variables)
          - [Pull multiple sims](#pull-multiple-sims)
      - [Quantifying deviations from abundance-driven
        change](#quantifying-deviations-from-abundance-driven-change)
          - [Fit brm](#fit-brm)
          - [Interpret brm](#interpret-brm)

# One route

We will work with the BBS route for Granby, CT to demonstrate the
workflow that gets applied to all the routes. (Just RMD’s usual guinea
pig route).

## Pull and examine raw data

``` r
dat <- granby

dat_isd <- just_isd(granby)

dat_to_plot <- dat_isd %>%
  mutate(energy = estimate_b(mass)) %>%
  filter(year %in% c(1988:1992, 2014:2018)) %>%
  group_by_all() %>%
  mutate(timeperiod = ifelse(year > 2000, "end", "begin")) %>%
  ungroup() %>%
  group_by(year) %>%
  summarize(
    total_abundance = dplyr::n(),
    total_energy = sum(energy),
    total_biomass = sum(mass)
  ) %>%
  ungroup() 
gridExtra::grid.arrange(grobs = list(ggplot(dat_to_plot, aes(year, total_abundance)) + geom_point() + ggtitle("abundance") + geom_smooth(method = "lm", se = F),
ggplot(dat_to_plot, aes(year, total_energy)) + geom_point() + ggtitle("energy")+ geom_smooth(method = "lm", se = F),
ggplot(dat_to_plot, aes(year, total_biomass)) + geom_point() + ggtitle("biomass")+ geom_smooth(method = "lm", se = F)), ncol = 3)
```

![](02_change_over_time_10sims_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

Granby looks like not an appreciable change in abundance, but
potentially an increase in energy and more probably biomass.

## Test for size-structure-driven decoupling

We want to test whether the changes in energy and biomass exceed what we
would expect to see if there had been **no change** in the size
structure from the begin to the end period.

From here on out, code is taken directly from
`View::rwar::draw_communities` and narrated.

### Arguments and explanation

`draw_communities(ts_comp, begin_years = 1988:1992, end_years
= 2014:2018, draw_seed = NULL, sampling_gmms = NULL, initial_isd_seed =
NULL, raw_isd_seed = NULL)`

``` r
ts_comp = dat
begin_years = 1988:1992
end_years = 2014:2018
draw_seed = NULL
sampling_gmms = NULL
inital_isd_seed = NULL
raw_isd_seed = NULL
```

### Construct sampling gmms

“Sampling GMMS” are density functions from Gaussian mixture models that
have a probability density for every mass along the spectrum of possible
masses (and then a large buffer). Drawing from `$mass` with probability
`$density` is a random number generator for sizes.

``` r
  # If GMM density smooths to sample from are not provided, get them
  # When running at scale, you want to provide them: 1) to ensure that all sims are being drawn from the same sampling GMMS, 2) for speed, it takes a long time to fit a GMM to a lot of ISD draws.

  if(is.null(sampling_gmms)) {

    sampling_gmms <- construct_sampling_gmm(ts_comp, begin_years = begin_years, end_years = end_years, initial_isd_seed = NULL)

  }
```

``` r
ggplot(sampling_gmms$begin, aes(mass, density)) + geom_line() + geom_line(data = sampling_gmms$end, color = "green") + ggtitle("Sampling GMMs", subtitle = "Black = begin, green = end") + xlab("Mass (log)")
```

![](02_change_over_time_10sims_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

### Get template ISD dataframes

``` r
  # This doesn't actually matter, I am using the raw isds to get the correct shape data frame. I am keeping it this way, even though it's inefficient, because in the back of my mind I think I might want to pull out raw estimates for biomass and energy use (i.e. drawing individuals directly from normal distributions for each year, rather than drawing from the ISD combining over all years). It'll be a tiny change but I'm not 100% sure what I want to do with that info yet so holding off.

  if(is.null(raw_isd_seed)) {
    set.seed(NULL)
    raw_isd_seed <- sample(1:1000000, 1)
  }

  # Here I am sampling ISDs to get dfs of the correct shape to then sample new body masses from different density fxns.

  raw_isd <- BBSsize::simulate_isd_ts(ts_comp, isd_seed = raw_isd_seed)$isd

  begin_isd <- dplyr::filter(raw_isd, year %in% begin_years) %>%
    dplyr::mutate(timeperiod = "begin")
  end_isd <- dplyr::filter(raw_isd, year %in% end_years) %>%
    dplyr::mutate(timeperiod = "end")
```

### Draw individuals corresponding to “sim” (no size change) and actual dynamics

``` r
  # Draw individuals for each time period from the MATCHING density functions
  # This will destroy interannual, intratimeperiod variation in the size structure, which we're OK with (the point of using 5-year intervals is to smooth out species accumulation)
  # I do not think it is a good idea to provide draw_seed, that will constrain things to come out the same in weird ways at this scale.
  begin_individuals <- add_drawn_individuals(begin_isd, sampling_gmms$begin, draw_seed = draw_seed)

  end_individuals <- add_drawn_individuals(end_isd, sampling_gmms$end, draw_seed = draw_seed)

  actual_individuals <- dplyr::bind_rows(begin_individuals, end_individuals) %>%
    dplyr::mutate(source = "currency")


  # Now draw individuals for each time period with scrambled ISDs. Specifically, draw for the beginning from the beginning ISD. But then also draw the end from the beginning ISD. This gives an "end" ISD pretending that the ISD didn't change from the beginning.
  begin_individuals_sim <- add_drawn_individuals(begin_isd, sampling_gmms$begin, draw_seed = draw_seed)

  end_individuals_sim <- add_drawn_individuals(end_isd, sampling_gmms$begin, draw_seed = draw_seed)


  sim_individuals <- dplyr::bind_rows(begin_individuals_sim, end_individuals_sim) %>%
    dplyr::mutate(source = "abundance")
```

### Combine with “raw”

The “raw” ISDs have not been in-and-out of the GMM smooth and then
resampling. The resampling is necessary to make the nullmodel/sims here
possible, but it masks intraannual variability in the size spectrum and
it can downweight (very slightly) the density of large individuals.

``` r
  # Go ahead and pull the raw state variable estimates too..

  raw_individuals <- dplyr::bind_rows(begin_isd, end_isd) %>%
    dplyr::mutate(energy = BBSsize::estimate_b(mass),
                  source = "raw",
                  isd_timeperiod = "raw",
                  sampling_seed = NA)

  all_individuals <- dplyr::bind_rows(actual_individuals, sim_individuals, raw_individuals)
```

### Compute time-period level state variables

``` r
  # Summarize individuals to get toal abundance, biomass, and energy use per year for each sim scenario.
  # And add route-level identifying info.
  all_svs <- all_individuals %>%
    dplyr::group_by(year, timeperiod, isd_timeperiod, sampling_seed, isd_seed, source) %>%
    dplyr::summarize(total_abundance = dplyr::n(),
                     total_biomass = sum(mass),
                     total_energy = sum(energy)) %>%
    dplyr::ungroup() %>%
    dplyr::bind_cols(as.data.frame(ts_comp$metadata$location)) %>%
    dplyr::mutate(matssname = paste0("bbs_rtrg_", route, "_", statenum))
```

``` r
gridExtra::grid.arrange(grobs = list(ggplot(all_svs, aes(year, total_abundance, color = source)) + geom_point() + geom_smooth(method = "lm", se= F) + ggtitle("abundance") + theme(legend.position = "bottom"),

ggplot(all_svs, aes(year, total_energy, color = source)) + geom_point() + geom_smooth(method = "lm", se= F) + ggtitle("energy") + theme(legend.position = "bottom"),

ggplot(all_svs, aes(year, total_biomass, color = source)) + geom_point() + geom_smooth(method = "lm", se= F) + ggtitle("biomass") + theme(legend.position = "bottom")), ncol = 3)
```

![](02_change_over_time_10sims_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

Some interpretation on these plots.

  - The lines are just lm smooths to help visualize the different
    groupings, not for strong inference particularly.
  - Note that the currencies are on different scales so the way the
    slopes appear to compare **between** plots is not informative.
  - We expect all of the abundance values to be the same because changes
    in total abundance are kept consistent through out all sims.
  - What we are actually interested in for understanding decoupling is
    the deviation between “actual” - red lines - and “sim” - blue lines
    - within plots for energy and biomass.
  - The “sim” shows the change in biomass/energy use that occurs if the
    size structure remains the same from beginning to end, i.e. only
    driven by the change in individual abundance between the two time
    periods.
  - The “actual” shows the change incorporating observed change in the
    size structure.
  - I include “raw” in these plots to illustrate the small bits of
    difference that come in between resampling from a GMM vs sampling
    the ISD directly.
  - For this route, increases in energy and biomass both deviate upwards
    from what would be expected just due to changes in abundance.

Note that these are one sim, and, because I haven’t fixed any of the
seeds in this document, it’s going to come out slightly differently each
time. There are sampling effects on the outcomes here.

We can pull multiple sims using `draw_communities_wrapper`.

### Pull multiple sims

``` r
multi_svs <- draw_communities_wrapper(granby, ndraws = 10, sampling_gmms = sampling_gmms) %>%
  mutate(group_to_plot = paste0(sim_iteration, source))
```

``` r
gridExtra::grid.arrange(grobs = list(ggplot(multi_svs, aes(year, total_abundance, color = source, group = as.factor(group_to_plot))) + geom_point() + geom_smooth(method = "lm", se= F, size = .5) + ggtitle("abundance") + theme(legend.position = "bottom"),
  ggplot(multi_svs, aes(year, total_energy, color = source, group = as.factor(group_to_plot))) + geom_point() + geom_smooth(method = "lm", se= F, size = .5) + ggtitle("energy") + theme(legend.position = "bottom"),
                                     ggplot(multi_svs, aes(year, total_biomass, color = source, group = as.factor(group_to_plot))) + geom_point() + geom_smooth(method = "lm", se= F, size = .5) + ggtitle("biomass") + theme(legend.position = "bottom")), ncol = 3)
```

![](02_change_over_time_10sims_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

``` r
gridExtra::grid.arrange(grobs = list(ggplot(filter(multi_svs, source != "raw"), aes(year, total_abundance, color = source, group = as.factor(group_to_plot))) + geom_point() + geom_smooth(method = "lm", se= F, size = .5) + ggtitle("abundance") + theme(legend.position = "bottom"),
  ggplot(filter(multi_svs, source != "raw"), aes(year, total_energy, color = source, group = as.factor(group_to_plot))) + geom_point() + geom_smooth(method = "lm", se= F, size = .5) + ggtitle("energy") + theme(legend.position = "bottom"),
                                     ggplot(filter(multi_svs, source != "raw"), aes(year, total_biomass, color = source, group = as.factor(group_to_plot))) + geom_point() + geom_smooth(method = "lm", se= F, size = .5) + ggtitle("biomass") + theme(legend.position = "bottom")), ncol = 3)
```

![](02_change_over_time_10sims_files/figure-gfm/unnamed-chunk-11-2.png)<!-- -->

From here we can see that there is some heterogeneity in the slopes
derived simply from repeating sims.

## Quantifying deviations from abundance-driven change

We would like to know:

  - What the slope is for the “sim” - abundance driven change
  - What the slope is for the “actual” - abundance + size structure
    driven change
  - If the slope for “actual” is different from sim
  - If so, by how much

We would like to incorporate:

  - There is variability year to year in total abundance (and therefore
    the other currencies) within each time period
  - There is uncertainty as shown by repeated sims

I am doing this using a brm because it allows me to extract estimates
for the parameters from the posterior and construct continuous CIs
around them (rather than having to use p-values and adjustments in a
frequentist setting).

I am using a brm of the form:

`brm(response ~ timeperiod * source + (1|year), data = one_route_data,
iter = 8000, thin = 2)`

  - timeperiod: this study is looking at things in a begin-end
    comparison. Using year gets you somewhere very similar, because the
    run (as in rise/run) is the same for all routes.
  - time period must interact with source, this is the piece we really
    care about.
  - we expect measurements within years to be \~artificially similar to
    each other because measurements for each year represent the same n
    draws where n in the number of individuals for that year
  - 8000 iterations has been enough to swamp convergence issues
  - thin to keep the resulting posteriors manageable

I am fitting individual brms to each route (as opposed to a massive
model with a group effect of route). I am doing this because it is
hugely computationally intensive to fit a brm to the whole dataset with
the correct model formula (i.e. allowing the slope and intercept to vary
for each source for each route, and incorporate year as a random
factor). It’s not practical to do it for more than a couple of sims per
route. I believe it is more important to have the correct formula and a
lot of sims than to get everything in one model.

### Fit brm

The following is from `fit_brms`:

Arguments:

``` r
some_sims = multi_svs
cores = 4
iter = 4000
thin = 1
```

This section is only for drake pipelines and will hopefully be able to
go once I clean up rwar:

``` r
#fit_brms <- function(some_sims, cores = 1, iter = 8000, thin =2) {

  # something in rwar as I currently have it is locking the namespace and interfering with drake, at least locally. this is not my best work but it gets rwar out of the namespace if it's attached.
  is_rwar_attached = any(grepl("rwar", names(sessionInfo()[7]$otherPkgs)))
  if(is_rwar_attached) {
    detach("package:rwar", unload = T)
  }
```

``` r
  # sims returns estimates of the raw values, which we don't want for the model fit (we jsut want the ones that come from drawing from the densityGMMS)
  justsims <- dplyr::filter(some_sims, source %in% c("abundance", "currency")) # remove raw


  # Fit a brm on total_energy
  te_brm <- brms::brm(total_energy ~ (timeperiod * source) + (1 | year), data = justsims, cores = cores, iter = iter, thin = thin)

  # Fit the brm on total_biomass
  tb_brm <- brms::brm(total_biomass ~ (timeperiod * source) + (1 | year), data = justsims, cores = cores, iter = iter, thin = thin)

  # keep track of what dataset this is
  md <- some_sims$matssname[1]

  # return(list(
  #   te_brm = te_brm,
  #   tb_brm = tb_brm,
  #   matssname =md
  # ))
some_brms <- (list(
    te_brm = te_brm,
    tb_brm = tb_brm,
    matssname =md
  ))
  
# }
```

### Interpret brm

OK, so this is new. I think parsimony is to extract the posterior draws
and then just look at the slope terms. And hey, the intercepts.

#### Total energy

``` r
library(tidybayes)


te_draws <- tidybayes::tidy_draws(te_brm) %>%
  tidybayes::median_qi(b_sourcecurrency, b_timeperiodend, `b_timeperiodend:sourcecurrency`, currency_slope =  `b_timeperiodend:sourcecurrency` + b_timeperiodend, relative_offset =  `b_timeperiodend:sourcecurrency` / b_timeperiodend, .width = c(.99, .95, .8))

ggplot(te_draws, aes(1, x = b_sourcecurrency, xmin = b_sourcecurrency.lower, xmax = b_sourcecurrency.upper)) + geom_pointinterval() + ggtitle("Currency intercept - should be on 0") + geom_vline(xintercept = 0)
```

![](02_change_over_time_10sims_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

``` r
ggplot(te_draws, aes(1, x = b_timeperiodend, xmin = b_timeperiodend.lower, xmax = b_timeperiodend.upper)) + geom_pointinterval() + ggtitle("Slope for abundance") + geom_vline(xintercept = 0)
```

![](02_change_over_time_10sims_files/figure-gfm/unnamed-chunk-15-2.png)<!-- -->

``` r
ggplot(te_draws, aes(1, x = `b_timeperiodend:sourcecurrency`, xmin = `b_timeperiodend:sourcecurrency.lower`, xmax = `b_timeperiodend:sourcecurrency.upper`)) + geom_pointinterval() + ggtitle("Offset from currency") + geom_vline(xintercept = 0)
```

![](02_change_over_time_10sims_files/figure-gfm/unnamed-chunk-15-3.png)<!-- -->

``` r
ggplot(te_draws, aes(1, x = currency_slope, xmin = currency_slope.lower, xmax = currency_slope.upper)) + geom_pointinterval() + ggtitle("Slope for currency") + geom_vline(xintercept = 0)
```

![](02_change_over_time_10sims_files/figure-gfm/unnamed-chunk-15-4.png)<!-- -->

``` r
ggplot(te_draws, aes(1, x = relative_offset, xmin = currency_slope.lower, xmax = currency_slope.upper)) + geom_pointinterval() + ggtitle("Currency offset relative to abund slope") + geom_vline(xintercept = 0)
```

![](02_change_over_time_10sims_files/figure-gfm/unnamed-chunk-15-5.png)<!-- -->

#### Total biomass

``` r
tb_draws <- tidybayes::tidy_draws(tb_brm) %>%
  tidybayes::median_qi(b_sourcecurrency, b_timeperiodend, `b_timeperiodend:sourcecurrency`, currency_slope =  `b_timeperiodend:sourcecurrency` + b_timeperiodend, relative_offset =  `b_timeperiodend:sourcecurrency` / b_timeperiodend, .width = c(.99, .95, .8))

ggplot(tb_draws, aes(1, x = b_sourcecurrency, xmin = b_sourcecurrency.lower, xmax = b_sourcecurrency.upper)) + geom_pointinterval() + ggtitle("Currency intercept - should be on 0") + geom_vline(xintercept = 0)
```

![](02_change_over_time_10sims_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

``` r
ggplot(tb_draws, aes(1, x = b_timeperiodend, xmin = b_timeperiodend.lower, xmax = b_timeperiodend.upper)) + geom_pointinterval() + ggtitle("Slope for abundance") + geom_vline(xintercept = 0)
```

![](02_change_over_time_10sims_files/figure-gfm/unnamed-chunk-16-2.png)<!-- -->

``` r
ggplot(tb_draws, aes(1, x = `b_timeperiodend:sourcecurrency`, xmin = `b_timeperiodend:sourcecurrency.lower`, xmax = `b_timeperiodend:sourcecurrency.upper`)) + geom_pointinterval() + ggtitle("Offset from currency") + geom_vline(xintercept = 0)
```

![](02_change_over_time_10sims_files/figure-gfm/unnamed-chunk-16-3.png)<!-- -->

``` r
ggplot(tb_draws, aes(1, x = currency_slope, xmin = currency_slope.lower, xmax = currency_slope.upper)) + geom_pointinterval() + ggtitle("Slope for currency") + geom_vline(xintercept = 0)
```

![](02_change_over_time_10sims_files/figure-gfm/unnamed-chunk-16-4.png)<!-- -->

#### On the same scales

To bring different currencies and routes into the same (ballpark)
scales, divide by the intercept. This won’t change whether these
quantities overlap 0, but they will bring them into the same ballpark
for visualization. Note this isn’t really for inference, but vis.

``` r
tb_draws_scaled <- tidybayes::tidy_draws(tb_brm) %>%
  tidybayes::median_qi(currency_intercept_scaled = b_sourcecurrency / b_Intercept, abundance_slope_scaled = b_timeperiodend / b_Intercept, currency_offset_scaled = `b_timeperiodend:sourcecurrency` / b_Intercept, currency_slope_scaled =  (`b_timeperiodend:sourcecurrency` + b_timeperiodend) / b_Intercept,  .width = c(.99, .95, .8))  %>%
  mutate(currency = "biomass")

te_draws_scaled <- tidybayes::tidy_draws(te_brm) %>%
  tidybayes::median_qi(currency_intercept_scaled = b_sourcecurrency / b_Intercept, abundance_slope_scaled = b_timeperiodend / b_Intercept, currency_offset_scaled = `b_timeperiodend:sourcecurrency` / b_Intercept, currency_slope_scaled =  (`b_timeperiodend:sourcecurrency` + b_timeperiodend) / b_Intercept, .width = c(.99, .95, .8))  %>%
  mutate(currency = "energy")

draws_scaled <- bind_rows(tb_draws_scaled, te_draws_scaled)


ggplot(draws_scaled, aes(currency, x = currency_intercept_scaled, xmin = currency_intercept_scaled.lower, xmax = currency_intercept_scaled.upper, color = currency)) + geom_pointinterval() + ggtitle("Currency intercept - should be on 0") + geom_vline(xintercept = 0)
```

![](02_change_over_time_10sims_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

``` r
ggplot(draws_scaled, aes(currency, x = abundance_slope_scaled, xmin = abundance_slope_scaled.lower, xmax = abundance_slope_scaled.upper)) + geom_pointinterval() + ggtitle("Slope for abundance") + geom_vline(xintercept = 0)
```

![](02_change_over_time_10sims_files/figure-gfm/unnamed-chunk-17-2.png)<!-- -->

``` r
ggplot(draws_scaled, aes(currency, x = currency_offset_scaled, xmin = currency_offset_scaled.lower, xmax = currency_offset_scaled.upper)) + geom_pointinterval() + ggtitle("Offset from currency") + geom_vline(xintercept = 0)
```

![](02_change_over_time_10sims_files/figure-gfm/unnamed-chunk-17-3.png)<!-- -->

``` r
ggplot(draws_scaled, aes(currency, x = currency_slope_scaled, xmin = currency_slope_scaled.lower, xmax = currency_slope_scaled.upper)) + geom_pointinterval() + ggtitle("Slope for currency") + geom_vline(xintercept = 0)
```

![](02_change_over_time_10sims_files/figure-gfm/unnamed-chunk-17-4.png)<!-- -->

Biomass over zeros:

``` r
mean_o_zero <- function(vector) {
  return(mean(vector > 0))
}

tb_draws <- tidybayes::tidy_draws(tb_brm) %>%
  select(b_Intercept, b_timeperiodend, `b_timeperiodend:sourcecurrency`, `b_sourcecurrency`) %>%
  summarize_all(mean_o_zero)

print(tb_draws)
```

    ## # A tibble: 1 × 4
    ##   b_Intercept b_timeperiodend `b_timeperiodend:sourcecurrency` b_sourcecurrency
    ##         <dbl>           <dbl>                            <dbl>            <dbl>
    ## 1           1           0.779                                1            0.258

``` r
print(summary(tb_brm))
```

    ##  Family: gaussian 
    ##   Links: mu = identity; sigma = identity 
    ## Formula: total_biomass ~ (timeperiod * source) + (1 | year) 
    ##    Data: justsims (Number of observations: 200) 
    ##   Draws: 4 chains, each with iter = 4000; warmup = 2000; thin = 1;
    ##          total post-warmup draws = 8000
    ## 
    ## Group-Level Effects: 
    ## ~year (Number of levels: 10) 
    ##               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)  3070.37    890.00  1805.46  5270.66 1.00     2167     3763
    ## 
    ## Population-Level Effects: 
    ##                              Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS
    ## Intercept                    34905.91   1425.17 32114.56 37778.77 1.00     3055
    ## timeperiodend                 1477.03   2074.67 -2754.41  5628.10 1.00     3104
    ## sourcecurrency                -377.46    574.51 -1516.42   743.81 1.00     6025
    ## timeperiodend:sourcecurrency  8220.47    811.11  6676.86  9805.55 1.00     6339
    ##                              Tail_ESS
    ## Intercept                        3864
    ## timeperiodend                    3816
    ## sourcecurrency                   5554
    ## timeperiodend:sourcecurrency     5852
    ## 
    ## Family Specific Parameters: 
    ##       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sigma  2881.18    149.01  2609.28  3198.36 1.00     6234     4913
    ## 
    ## Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

Energy over zeros:

``` r
te_draws <- tidybayes::tidy_draws(te_brm) %>%
  select(b_Intercept, b_timeperiodend, `b_timeperiodend:sourcecurrency`, `b_sourcecurrency`) %>%
  summarize_all(mean_o_zero)

print(te_draws)
```

    ## # A tibble: 1 × 4
    ##   b_Intercept b_timeperiodend `b_timeperiodend:sourcecurrency` b_sourcecurrency
    ##         <dbl>           <dbl>                            <dbl>            <dbl>
    ## 1           1           0.801                                1            0.212

``` r
print(summary(te_brm))
```

    ##  Family: gaussian 
    ##   Links: mu = identity; sigma = identity 
    ## Formula: total_energy ~ (timeperiod * source) + (1 | year) 
    ##    Data: justsims (Number of observations: 200) 
    ##   Draws: 4 chains, each with iter = 4000; warmup = 2000; thin = 1;
    ##          total post-warmup draws = 8000
    ## 
    ## Group-Level Effects: 
    ## ~year (Number of levels: 10) 
    ##               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)  8321.32   2282.99  5153.30 14019.49 1.00     2570     3722
    ## 
    ## Population-Level Effects: 
    ##                              Estimate Est.Error l-95% CI  u-95% CI Rhat
    ## Intercept                    99664.35   3732.15 92193.18 106958.71 1.00
    ## timeperiodend                 4301.41   5471.80 -6622.25  15222.51 1.00
    ## sourcecurrency                -711.54    887.34 -2473.01   1011.65 1.00
    ## timeperiodend:sourcecurrency  7641.77   1253.71  5227.99  10094.93 1.00
    ##                              Bulk_ESS Tail_ESS
    ## Intercept                        2716     3563
    ## timeperiodend                    2784     3265
    ## sourcecurrency                   4980     4880
    ## timeperiodend:sourcecurrency     5309     4834
    ## 
    ## Family Specific Parameters: 
    ##       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sigma  4367.92    224.03  3956.96  4837.03 1.00     6329     5091
    ## 
    ## Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).
Modeling approach & validation
================
Renata Diaz
2021-11-01

  - [How we get here](#how-we-get-here)
  - [Data](#data)
  - [Models](#models)
      - [Fit models](#fit-models)
      - [Compare models](#compare-models)
      - [Select best model](#select-best-model)
  - [Demo scenarios](#demo-scenarios)

# How we get here

See 02\_01\_change\_over\_time\_sims.Rmd for an explanation of how we
generate sims describing energy use/biomass dynamics assuming a) actual
change in the size structure over time or b) no change in the size
structure (such that any change that occurs is driven solely by changes
in abundance).

From there, we would like to know:

  - how much change has occurred due to change in abundance
  - how much change in the currency deviates from the change driven by
    change in abundance (reflecting change in the size structure)

# Data

We use the sims method to get estimates of total energy or total biomass
for each year under the scenarios 1) actual size structure change 2) no
change, achieved by using the size structure for the first 5 years for
the last 5 years as well.

``` r
#dat = granby

#actual_sims <- rwar::make_actual_sims(dat) # this is a wrapper fxns that draws 100 sims

print(rwar::make_actual_sims)
```

This gives us a dataframe with columns:

``` r
#colnames(actual_sims)
```

``` r
#ggplot(actual_sims, aes(year, total_biomass, color = source)) + geom_point() + geom_smooth(method = "lm", se = F)
```

We want to know:

  - the slope for abundance
  - the slope for currency
  - i.e. whether the slope for currency is different from the slope for
    abundance

To fit models, we need to summarize across all the sims, because
including all the individual sims is going to artificially inflate the
model’s confidence.

We also want to remove “raw” because that is just for a gut check.

``` r
#ssims_actual_sims <- rwar::summarize_sims(actual_sims)

print(rwar::summarize_sims)
```

    ## function(sims) {
    ## 
    ##   sims <- sims %>% dplyr::filter(source != "raw") %>%
    ##     dplyr::group_by(timeperiod, source, year, matssname,simtype) %>%
    ##     dplyr::summarize(total_energy = mean(total_energy),
    ##                      total_biomass = mean(total_biomass),
    ##                      ndraws = dplyr::n()) %>%
    ##     dplyr::ungroup()
    ## 
    ##   return(sims)
    ## 
    ## }
    ## <bytecode: 0x7fd1f6c7e580>
    ## <environment: namespace:rwar>

``` r
#head(ssims_actual_sims)
```

# Models

We will use bayesian linear models to estimate the slopes for abundance
and currency and the difference between them.

We will compare models of the following forms:

`response ~ timeperiod * source`: Estimates a slope for timeperiod
(“begin” or “end”) and the interaction between source (“abundance” or
“currency”)

`response ~ timeperiod`: Only a timeperiod slope, no difference between
sources

`response ~ 1`: Only an intercept, no change begin or end.

We will use the LOO criterion from `rstanarm::loo`. Because sometimes
the best model according to LOO wins by a very small margin, we will
select the simplest model within 1 standard error of the best model for
interpretation.

## Fit models

``` r
print(rwar::fit_stanlm)
```

    ## function(some_sims) {
    ## 
    ##   # Fit a brm on total_energy
    ##   te_brm_full <- rstanarm::stan_glm(total_energy ~ (timeperiod * source) , data = some_sims, iter =8000, thin = 4)
    ##   te_brm_nosource <- rstanarm::stan_glm(total_energy ~ (timeperiod), data = some_sims, iter =8000, thin = 4)
    ##   te_brm_notime <- rstanarm::stan_glm(total_energy ~ 1, data = some_sims, iter =8000, thin = 4)
    ## 
    ##   te_brms = list(
    ##     te_brm_full = te_brm_full,
    ##     te_brm_nosource = te_brm_nosource,
    ##     te_brm_notime = te_brm_notime
    ##   )
    ## 
    ## 
    ##   # Fit the brm on total_biomass
    ##   tb_brm_full <- rstanarm::stan_glm(total_biomass ~ (timeperiod * source) , data = some_sims, iter = 8000, thin = 4)
    ##   tb_brm_nosource <- rstanarm::stan_glm(total_biomass ~ (timeperiod) , data = some_sims, iter = 8000, thin = 4)
    ##   tb_brm_notime <- rstanarm::stan_glm(total_biomass ~ 1 , data = some_sims, iter = 8000, thin = 4)
    ## 
    ## 
    ##   tb_brms = list(
    ##     tb_brm_full = tb_brm_full,
    ##     tb_brm_nosource = tb_brm_nosource,
    ##     tb_brm_notime = tb_brm_notime
    ##   )
    ## 
    ##   return(list(
    ##     te_brms = te_brms,
    ##     tb_brms = tb_brms,
    ##     matssname =some_sims$matssname[1],
    ##     simtype = some_sims$simtype[1]
    ##   ))
    ## 
    ## }
    ## <bytecode: 0x7fd1f6da8d28>
    ## <environment: namespace:rwar>

## Compare models

``` r
#fits_compare <- rwar::compare_both_brms(fits)

print(rwar::compare_both_stanarms)
```

    ## function(some_brms_fits) {
    ## 
    ##   biomass <- compare_stanarms(brms_fits = some_brms_fits$tb_brms)
    ##   energy <- compare_stanarms(brms_fits = some_brms_fits$te_brms)
    ## 
    ##   both_comparisons <- dplyr::bind_rows(biomass = biomass, energy = energy, .id = "currency") %>%
    ##     dplyr::mutate(matssname = some_brms_fits$matssname,
    ##                   simtype = some_brms_fits$simtype[1])
    ## 
    ## 
    ##   return(both_comparisons)
    ## 
    ## }
    ## <bytecode: 0x7fd1d97e72a0>
    ## <environment: namespace:rwar>

``` r
print(rwar::compare_stanarms)
```

    ## function(brms_fits) {
    ## 
    ##   brms_loos<- lapply(brms_fits, rstanarm::loo, k_threshold= .7)
    ## 
    ##   brms_comparison <- rstanarm::loo_compare(brms_loos) %>%
    ##     as.data.frame() %>%
    ##     dplyr::mutate(model = row.names(.),
    ##                   rank = dplyr::row_number())
    ## 
    ##   return(brms_comparison)
    ## 
    ## }
    ## <bytecode: 0x7fd1da0566a8>
    ## <environment: namespace:rwar>

## Select best model

``` r
#fits_winners <- rwar::loo_select(fits_compare)

print(rwar::loo_select)
```

    ## function(some_compares) {
    ## 
    ## 
    ##   winners <- some_compares %>%
    ##     dplyr::mutate(model_complexity = ifelse(grepl("full", model), 3,
    ##                                      ifelse(grepl("source", model), 2, 1))) %>%
    ##     dplyr::arrange(matssname, currency, simtype, rank) %>%
    ##     dplyr::group_by(matssname, currency, simtype) %>%
    ##     # dplyr::mutate(best_elpd = elpd_loo[1],
    ##     #               best_se = se_elpd_loo[1]) %>%
    ##     # dplyr::ungroup() %>%
    ##     # dplyr::mutate(one_se_cutoff = best_elpd - best_se) %>%
    ##     dplyr::mutate(in_one_se = (elpd_diff + se_diff) >= 0) %>%
    ##     dplyr::filter(in_one_se) %>%
    ##     dplyr::group_by(currency) %>%
    ##     dplyr::arrange(model_complexity) %>%
    ##     dplyr::mutate(model_rank = dplyr::row_number()) %>%
    ##     dplyr::ungroup() %>%
    ##     dplyr::filter(model_rank == 1)
    ## 
    ##   return(winners)
    ## }
    ## <bytecode: 0x7fd1edd096c8>
    ## <environment: namespace:rwar>

``` r
#print(fits_winners)
```

# Demo scenarios

I ran this pipeline on 4 datasets demonstrating different types of
dynamics, and compared the “actual” dynamics to additional sims
illustrating different “real” effects.

``` r
library(drake)

## Set up the cache and config
db <- DBI::dbConnect(RSQLite::SQLite(), here::here("aspirational_structure", "drake_caches", "sim_means-cache-unlocked.sqlite"))
cache <- storr::storr_dbi("datatable", "keystable", db)
cache$del(key = "lock", namespace = "session")

#cached(cache = cache)
loadd(all_sims, cache=cache)

all_sims <- all_sims %>%
  filter(matssname %in% c("bbs_rtrg_116_18", "bbs_rtrg_19_7", "bbs_rtrg_224_3", "bbs_rtrg_318_3"))
```

``` r
ggplot(all_sims, aes(year, total_biomass, color = source)) + geom_point() + geom_smooth(method = "lm", se = F, size = .3) + facet_wrap(vars(matssname, simtype), scales = "free", ncol = 3) + theme(legend.position = "bottom")
```

    ## `geom_smooth()` using formula 'y ~ x'

![](02_02_testing_change_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

The “sims” correspond to: no change in abunance or size strucutre (no
change), no change in size structure (nosizechange). While we expect to
get decoupling of currency and size structure in some actual dynamics,
we do not expected it in these sims. We have weaker expectations about
the slopes of the actual dynamics. Note that the lines are lm smooths
without se and not suitable for inference.

Here are the model winners for the different sims:

224 and 116 both have deviations; 19\_7 selects no change; 318\_3
selects a slope but not a deviation. None of the sims select
overly-complex models. Note that usually but not often we’re invoking
the one se rule (if `rank` isn’t 1 it means we selected a lower-ranked
model because it was simpler and within 1 se.)

``` r
loadd(all_winners, cache = cache)

all_winners %>%
  filter(matssname %in% c("bbs_rtrg_116_18", "bbs_rtrg_19_7", "bbs_rtrg_224_3", "bbs_rtrg_318_3")) %>%
  dplyr::filter(currency == "biomass")
```

<div class="kable-table">

| currency | elpd\_diff | se\_diff |  elpd\_loo | se\_elpd\_loo |   p\_loo | se\_p\_loo |    looic | se\_looic | model             | rank | matssname          | simtype      | model\_complexity | in\_one\_se | model\_rank |
| :------- | ---------: | -------: | ---------: | ------------: | -------: | ---------: | -------: | --------: | :---------------- | ---: | :----------------- | :----------- | ----------------: | :---------- | ----------: |
| biomass  |   0.000000 | 0.000000 | \-220.0444 |      2.501839 | 4.212340 |  0.8800493 | 440.0888 |  5.003678 | tb\_brm\_full     |    1 | bbs\_rtrg\_224\_3  | actual       |                 3 | TRUE        |           1 |
| biomass  |   0.000000 | 0.000000 | \-183.1219 |      2.690652 | 2.550404 |  0.7021617 | 366.2438 |  5.381303 | tb\_brm\_nosource |    1 | bbs\_rtrg\_318\_3  | actual       |                 2 | TRUE        |           1 |
| biomass  | \-1.133523 | 2.667399 | \-220.4480 |      2.337003 | 1.424376 |  0.3881390 | 440.8960 |  4.674007 | tb\_brm\_notime   |    3 | bbs\_rtrg\_19\_7   | actual       |                 1 | TRUE        |           1 |
| biomass  |   0.000000 | 0.000000 | \-190.1926 |      2.722858 | 4.313543 |  1.0720598 | 380.3852 |  5.445716 | tb\_brm\_full     |    1 | bbs\_rtrg\_116\_18 | actual       |                 3 | TRUE        |           1 |
| biomass  |   0.000000 | 0.000000 | \-215.9217 |      2.861991 | 1.649904 |  0.4317976 | 431.8434 |  5.723981 | tb\_brm\_notime   |    1 | bbs\_rtrg\_224\_3  | nochange     |                 1 | TRUE        |           1 |
| biomass  |   0.000000 | 0.000000 | \-177.7140 |      1.902237 | 1.303967 |  0.2125263 | 355.4281 |  3.804474 | tb\_brm\_notime   |    1 | bbs\_rtrg\_318\_3  | nochange     |                 1 | TRUE        |           1 |
| biomass  |   0.000000 | 0.000000 | \-218.9831 |      1.068621 | 1.073560 |  0.0783536 | 437.9662 |  2.137243 | tb\_brm\_notime   |    1 | bbs\_rtrg\_19\_7   | nochange     |                 1 | TRUE        |           1 |
| biomass  |   0.000000 | 0.000000 | \-184.4094 |      1.869772 | 1.280764 |  0.2019415 | 368.8188 |  3.739543 | tb\_brm\_notime   |    1 | bbs\_rtrg\_116\_18 | nochange     |                 1 | TRUE        |           1 |
| biomass  |   0.000000 | 0.000000 | \-214.6721 |      2.869987 | 2.501670 |  0.6996107 | 429.3443 |  5.739973 | tb\_brm\_nosource |    1 | bbs\_rtrg\_224\_3  | nosizechange |                 2 | TRUE        |           1 |
| biomass  |   0.000000 | 0.000000 | \-181.1279 |      2.018550 | 2.206306 |  0.4127268 | 362.2558 |  4.037101 | tb\_brm\_nosource |    1 | bbs\_rtrg\_318\_3  | nosizechange |                 2 | TRUE        |           1 |
| biomass  |   0.000000 | 0.000000 | \-217.9803 |      1.726628 | 2.175301 |  0.2875843 | 435.9606 |  3.453255 | tb\_brm\_nosource |    1 | bbs\_rtrg\_19\_7   | nosizechange |                 2 | TRUE        |           1 |
| biomass  |   0.000000 | 0.000000 | \-185.9874 |      1.802748 | 1.252506 |  0.1732748 | 371.9748 |  3.605495 | tb\_brm\_notime   |    1 | bbs\_rtrg\_116\_18 | nosizechange |                 1 | TRUE        |           1 |

</div>

For model diagnostics and results see `results/results_reports/`

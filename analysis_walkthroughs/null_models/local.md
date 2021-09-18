Local null model
================

For the local-scale null model we rearrange the species IDs **of species
that have been recorded along a route**.

# Get some data

``` r
h <- BBSsize::hartland
```

# Rearrange species

``` r
set.seed(1994)

orig_species <- colnames(h$abundance)

shuffled_species <- sample(orig_species, size = length(orig_species), replace = F)

h_shuffled <- h

colnames(h_shuffled$abundance) <- shuffled_species
```

# Run analyses

``` r
library(BBSsize)

h_analyses <- all_core_analyses(h, 1994:1998, 2014:2018, 1977)
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

    ## Package 'mclust' version 5.4.7
    ## Type 'citation("mclust")' for citing this R package in publications.

``` r
h_shuffled_analyses <- all_core_analyses(h_shuffled, 1994:1998, 2014:2018, 1977)
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

``` r
two <- bind_rows(h_analyses, h_shuffled_analyses)
```

## step by step

``` r
ts_isd_actual <- BBSsize::simulate_isd_ts(h, isd_seed = 1977)
ts_svs_actual <- get_annual_svs(ts_isd_actual$isd)
ts_lms_actual <- fit_all_timeseries_lms(ts_svs_actual)
caps_svs_actual <- pull_caps(ts_svs_actual, 1994:1998, 2014:2018)
caps_lms_actual <- fit_all_caps_lms(caps_svs_actual)
i_lms_actual <- interaction_lms(caps_svs_actual)
raw_ratios_actual <-  compute_raw_sv_change(caps_svs_actual)
set.seed(1977)
isd_turn_actual <- compare_isds(ts_isd_actual$isd, 1994:1998, 2014:2018)
comp_turn_actual <- compare_species_composition(h, 1994:1998, 2014:2018)

ts_isd_shuffled <- BBSsize::simulate_isd_ts(h_shuffled, isd_seed = 1977)
ts_svs_shuffled <- get_annual_svs(ts_isd_shuffled$isd)
ts_lms_shuffled <- fit_all_timeseries_lms(ts_svs_shuffled)
caps_svs_shuffled <- pull_caps(ts_svs_shuffled, 1994:1998, 2014:2018)
caps_lms_shuffled <- fit_all_caps_lms(caps_svs_shuffled)
i_lms_shuffled <- interaction_lms(caps_svs_shuffled)
raw_ratios_shuffled <-  compute_raw_sv_change(caps_svs_shuffled)
set.seed(1977)
isd_turn_shuffled <- compare_isds(ts_isd_shuffled$isd, 1994:1998, 2014:2018)
comp_turn_shuffled <- compare_species_composition(h_shuffled, 1994:1998, 2014:2018)


isds <- bind_rows(list(actual = ts_isd_actual$isd, shuffled =ts_isd_shuffled$isd), .id = "source")

ggplot(isds, aes(log(mass), fill = year > 2000)) + geom_density() + facet_wrap(vars(source))
```

# Repeatedly for a null model

``` r
local_null_model <- function(ts_dat, null_mod_seed = NULL, begin_years = NULL, end_years = NULL, isd_seed = NULL) {
  
  if(is.null(null_mod_seed)) {
    set.seed(NULL)
    null_mod_seed <- sample.int(1000000000, 1)
  }
  
  
  orig_species <- colnames(ts_dat$abundance)
  
  set.seed(null_mod_seed)
  
  shuffled_species <- sample(orig_species, size = length(orig_species), replace = F)
  
  shuffled_dat <- ts_dat
  
  colnames(shuffled_dat$abundance) <- shuffled_species
  
  results <- all_core_analyses(shuffled_dat, begin_years, end_years, isd_seed)
  
  results <- results %>%
    dplyr::mutate(
      null_mod_type = "local",
      null_mod_seed = null_mod_seed
    )
  
 
  results 
}


r3 <- replicate(5, local_null_model(h, NULL, 1994:1998, 2014:2018, 1977), simplify = FALSE)
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
five_nulls <- bind_rows(r3)
```

Always want to check that:

  - Results for total abundance and speices turnover remain the same
  - Multiple, different null\_mod\_seeds in multiple runs

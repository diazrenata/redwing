Regional null model
================

``` r
g <- granby

g_locationdat <- g$metadata$location

g_speciesoverlap <- dplyr::left_join(g_locationdat, route_intersections)
```

    ## Joining, by = c("countrynum", "statenum", "route", "routename", "active", "latitude", "longitude", "stratum", "bcr", "routetypeid", "routetypedetailid", "regionname")

``` r
overlapping_spp <- g_speciesoverlap$id
encountered_spp <- g$metadata$species_table$id

overlap_rich = length(overlapping_spp)
encountered_rich = length(encountered_spp)
overlap_not_in_encountered = sum(!(overlapping_spp %in% encountered_spp))
all_g_species=c(overlapping_spp,encountered_spp) %>% unique()
orig_g_species <- colnames(g$abundance)
set.seed(1994)
new_g_species <- sample(all_g_species, size = length(orig_g_species), replace = F)

g_shuffled <- g

colnames(g_shuffled$abundance) <- new_g_species
```

# Run analyses

``` r
library(BBSsize)

g_analyses <- all_core_analyses(g, startyears, endyears, isd_seed)
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

    ## Package 'mclust' version 5.4.7
    ## Type 'citation("mclust")' for citing this R package in publications.

``` r
g_shuffled_analyses <- all_core_analyses(g_shuffled, startyears, endyears, isd_seed)
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

``` r
two <- bind_rows(g_analyses, g_shuffled_analyses)
```

## step by step

``` r
ts_isd_actual <- BBSsize::simulate_isd_ts(g, isd_seed = isd_seed)
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

``` r
ts_svs_actual <- get_annual_svs(ts_isd_actual$isd)
ts_lms_actual <- fit_all_timeseries_lms(ts_svs_actual)
caps_svs_actual <- pull_caps(ts_svs_actual, startyears, endyears)
caps_lms_actual <- fit_all_caps_lms(caps_svs_actual)
i_lms_actual <- interaction_lms(caps_svs_actual)
raw_ratios_actual <-  compute_raw_sv_change(caps_svs_actual)
set.seed(isd_seed)
isd_turn_actual <- compare_isds(ts_isd_actual$isd, startyears, endyears)
comp_turn_actual <- compare_species_composition(g, startyears, endyears)

ts_isd_shuffled <- BBSsize::simulate_isd_ts(g_shuffled, isd_seed = isd_seed)
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

``` r
ts_svs_shuffled <- get_annual_svs(ts_isd_shuffled$isd)
ts_lms_shuffled <- fit_all_timeseries_lms(ts_svs_shuffled)
caps_svs_shuffled <- pull_caps(ts_svs_shuffled, startyears, endyears)
caps_lms_shuffled <- fit_all_caps_lms(caps_svs_shuffled)
i_lms_shuffled <- interaction_lms(caps_svs_shuffled)
raw_ratios_shuffled <-  compute_raw_sv_change(caps_svs_shuffled)
set.seed(1994)
isd_turn_shuffled <- compare_isds(ts_isd_shuffled$isd, startyears, endyears)
comp_turn_shuffled <- compare_species_composition(g_shuffled, startyears, endyears)


isds <- bind_rows(list(actual = ts_isd_actual$isd, shuffled =ts_isd_shuffled$isd), .id = "source")

ggplot(isds, aes(log(mass), fill = year > 2000)) + geom_density(alpha = .5) + facet_wrap(vars(source))
```

![](regional_files/figure-gfm/unnamed-chunk-3-1.png)<!-- --> \#
Repeatedly for a null model

``` r
regional_null_model <- function(ts_dat, ranges_dat = NULL, null_mod_seed = NULL, begin_years = NULL, end_years = NULL, isd_seed = NULL) {
  
  if(is.null(ranges_dat)) {
    stop("Need range data")
  }
  
  if(is.null(null_mod_seed)) {
    set.seed(NULL)
    null_mod_seed <- sample.int(1000000000, 1)
  }
  
  
  ts_locationdat <- ts_dat$metadata$location
  
  ts_speciesoverlap <- dplyr::left_join(ts_locationdat, ranges_dat)
  
  overlapping_spp <- ts_speciesoverlap$id
  encountered_spp <- ts_dat$metadata$species_table$id
  all_ts_spp = c(overlapping_spp, encountered_spp) %>% unique()
  
  overlap_rich = length(overlapping_spp)
  encountered_rich = length(encountered_spp)
  overlap_not_in_encountered = sum(!(overlapping_spp %in% encountered_spp))

  set.seed(null_mod_seed)
  
  new_ts_species <- sample(all_ts_spp, size = encountered_rich, replace = F)
  
  shuffled_dat <- ts_dat
  
  colnames(shuffled_dat$abundance) <- new_ts_species
  
  results <- all_core_analyses(shuffled_dat, begin_years, end_years, isd_seed)
  
  results <- results %>%
    dplyr::mutate(
      null_mod_type = "regional",
      null_mod_seed = null_mod_seed,
      overlap_richness = overlap_rich,
      local_richness = encountered_rich,
      regionally_added = overlap_not_in_encountered
    )
  
  
  results 
}
```

``` r
r3 <- replicate(50, regional_null_model(g, route_intersections, NULL, startyears, endyears, isd_seed), simplify = FALSE)
```

    ## Joining, by = c("countrynum", "statenum", "route", "routename", "active", "latitude", "longitude", "stratum", "bcr", "routetypeid", "routetypedetailid", "regionname")

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

    ## Joining, by = c("countrynum", "statenum", "route", "routename", "active", "latitude", "longitude", "stratum", "bcr", "routetypeid", "routetypedetailid", "regionname")

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

    ## Joining, by = c("countrynum", "statenum", "route", "routename", "active", "latitude", "longitude", "stratum", "bcr", "routetypeid", "routetypedetailid", "regionname")

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

    ## Joining, by = c("countrynum", "statenum", "route", "routename", "active", "latitude", "longitude", "stratum", "bcr", "routetypeid", "routetypedetailid", "regionname")

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

    ## Joining, by = c("countrynum", "statenum", "route", "routename", "active", "latitude", "longitude", "stratum", "bcr", "routetypeid", "routetypedetailid", "regionname")

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

    ## Joining, by = c("countrynum", "statenum", "route", "routename", "active", "latitude", "longitude", "stratum", "bcr", "routetypeid", "routetypedetailid", "regionname")

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

    ## Joining, by = c("countrynum", "statenum", "route", "routename", "active", "latitude", "longitude", "stratum", "bcr", "routetypeid", "routetypedetailid", "regionname")

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

    ## Joining, by = c("countrynum", "statenum", "route", "routename", "active", "latitude", "longitude", "stratum", "bcr", "routetypeid", "routetypedetailid", "regionname")

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

    ## Joining, by = c("countrynum", "statenum", "route", "routename", "active", "latitude", "longitude", "stratum", "bcr", "routetypeid", "routetypedetailid", "regionname")

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

    ## Joining, by = c("countrynum", "statenum", "route", "routename", "active", "latitude", "longitude", "stratum", "bcr", "routetypeid", "routetypedetailid", "regionname")

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

    ## Joining, by = c("countrynum", "statenum", "route", "routename", "active", "latitude", "longitude", "stratum", "bcr", "routetypeid", "routetypedetailid", "regionname")

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

    ## Joining, by = c("countrynum", "statenum", "route", "routename", "active", "latitude", "longitude", "stratum", "bcr", "routetypeid", "routetypedetailid", "regionname")

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

    ## Joining, by = c("countrynum", "statenum", "route", "routename", "active", "latitude", "longitude", "stratum", "bcr", "routetypeid", "routetypedetailid", "regionname")

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

    ## Joining, by = c("countrynum", "statenum", "route", "routename", "active", "latitude", "longitude", "stratum", "bcr", "routetypeid", "routetypedetailid", "regionname")

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

    ## Joining, by = c("countrynum", "statenum", "route", "routename", "active", "latitude", "longitude", "stratum", "bcr", "routetypeid", "routetypedetailid", "regionname")

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

    ## Joining, by = c("countrynum", "statenum", "route", "routename", "active", "latitude", "longitude", "stratum", "bcr", "routetypeid", "routetypedetailid", "regionname")

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

    ## Joining, by = c("countrynum", "statenum", "route", "routename", "active", "latitude", "longitude", "stratum", "bcr", "routetypeid", "routetypedetailid", "regionname")

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

    ## Joining, by = c("countrynum", "statenum", "route", "routename", "active", "latitude", "longitude", "stratum", "bcr", "routetypeid", "routetypedetailid", "regionname")

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

    ## Joining, by = c("countrynum", "statenum", "route", "routename", "active", "latitude", "longitude", "stratum", "bcr", "routetypeid", "routetypedetailid", "regionname")

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

    ## Joining, by = c("countrynum", "statenum", "route", "routename", "active", "latitude", "longitude", "stratum", "bcr", "routetypeid", "routetypedetailid", "regionname")

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

    ## Joining, by = c("countrynum", "statenum", "route", "routename", "active", "latitude", "longitude", "stratum", "bcr", "routetypeid", "routetypedetailid", "regionname")

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

    ## Joining, by = c("countrynum", "statenum", "route", "routename", "active", "latitude", "longitude", "stratum", "bcr", "routetypeid", "routetypedetailid", "regionname")

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

    ## Joining, by = c("countrynum", "statenum", "route", "routename", "active", "latitude", "longitude", "stratum", "bcr", "routetypeid", "routetypedetailid", "regionname")

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

    ## Joining, by = c("countrynum", "statenum", "route", "routename", "active", "latitude", "longitude", "stratum", "bcr", "routetypeid", "routetypedetailid", "regionname")

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

    ## Joining, by = c("countrynum", "statenum", "route", "routename", "active", "latitude", "longitude", "stratum", "bcr", "routetypeid", "routetypedetailid", "regionname")

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

    ## Joining, by = c("countrynum", "statenum", "route", "routename", "active", "latitude", "longitude", "stratum", "bcr", "routetypeid", "routetypedetailid", "regionname")

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

    ## Joining, by = c("countrynum", "statenum", "route", "routename", "active", "latitude", "longitude", "stratum", "bcr", "routetypeid", "routetypedetailid", "regionname")

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

    ## Joining, by = c("countrynum", "statenum", "route", "routename", "active", "latitude", "longitude", "stratum", "bcr", "routetypeid", "routetypedetailid", "regionname")

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

    ## Joining, by = c("countrynum", "statenum", "route", "routename", "active", "latitude", "longitude", "stratum", "bcr", "routetypeid", "routetypedetailid", "regionname")

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

    ## Joining, by = c("countrynum", "statenum", "route", "routename", "active", "latitude", "longitude", "stratum", "bcr", "routetypeid", "routetypedetailid", "regionname")

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

    ## Joining, by = c("countrynum", "statenum", "route", "routename", "active", "latitude", "longitude", "stratum", "bcr", "routetypeid", "routetypedetailid", "regionname")

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

    ## Joining, by = c("countrynum", "statenum", "route", "routename", "active", "latitude", "longitude", "stratum", "bcr", "routetypeid", "routetypedetailid", "regionname")

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

    ## Joining, by = c("countrynum", "statenum", "route", "routename", "active", "latitude", "longitude", "stratum", "bcr", "routetypeid", "routetypedetailid", "regionname")

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

    ## Joining, by = c("countrynum", "statenum", "route", "routename", "active", "latitude", "longitude", "stratum", "bcr", "routetypeid", "routetypedetailid", "regionname")

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

    ## Joining, by = c("countrynum", "statenum", "route", "routename", "active", "latitude", "longitude", "stratum", "bcr", "routetypeid", "routetypedetailid", "regionname")

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

    ## Joining, by = c("countrynum", "statenum", "route", "routename", "active", "latitude", "longitude", "stratum", "bcr", "routetypeid", "routetypedetailid", "regionname")

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

    ## Joining, by = c("countrynum", "statenum", "route", "routename", "active", "latitude", "longitude", "stratum", "bcr", "routetypeid", "routetypedetailid", "regionname")

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

    ## Joining, by = c("countrynum", "statenum", "route", "routename", "active", "latitude", "longitude", "stratum", "bcr", "routetypeid", "routetypedetailid", "regionname")

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

    ## Joining, by = c("countrynum", "statenum", "route", "routename", "active", "latitude", "longitude", "stratum", "bcr", "routetypeid", "routetypedetailid", "regionname")

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

    ## Joining, by = c("countrynum", "statenum", "route", "routename", "active", "latitude", "longitude", "stratum", "bcr", "routetypeid", "routetypedetailid", "regionname")

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

    ## Joining, by = c("countrynum", "statenum", "route", "routename", "active", "latitude", "longitude", "stratum", "bcr", "routetypeid", "routetypedetailid", "regionname")

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

    ## Joining, by = c("countrynum", "statenum", "route", "routename", "active", "latitude", "longitude", "stratum", "bcr", "routetypeid", "routetypedetailid", "regionname")

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

    ## Joining, by = c("countrynum", "statenum", "route", "routename", "active", "latitude", "longitude", "stratum", "bcr", "routetypeid", "routetypedetailid", "regionname")

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

    ## Joining, by = c("countrynum", "statenum", "route", "routename", "active", "latitude", "longitude", "stratum", "bcr", "routetypeid", "routetypedetailid", "regionname")

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

    ## Joining, by = c("countrynum", "statenum", "route", "routename", "active", "latitude", "longitude", "stratum", "bcr", "routetypeid", "routetypedetailid", "regionname")

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

    ## Joining, by = c("countrynum", "statenum", "route", "routename", "active", "latitude", "longitude", "stratum", "bcr", "routetypeid", "routetypedetailid", "regionname")

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

    ## Joining, by = c("countrynum", "statenum", "route", "routename", "active", "latitude", "longitude", "stratum", "bcr", "routetypeid", "routetypedetailid", "regionname")

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

    ## Joining, by = c("countrynum", "statenum", "route", "routename", "active", "latitude", "longitude", "stratum", "bcr", "routetypeid", "routetypedetailid", "regionname")

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

    ## Joining, by = c("countrynum", "statenum", "route", "routename", "active", "latitude", "longitude", "stratum", "bcr", "routetypeid", "routetypedetailid", "regionname")

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

    ## Joining, by = c("countrynum", "statenum", "route", "routename", "active", "latitude", "longitude", "stratum", "bcr", "routetypeid", "routetypedetailid", "regionname")

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
nulls <- bind_rows(r3)
```

Always want to check that:

  - Results for total abundance and species turnover remain the same
  - Multiple, different null\_mod\_seeds in multiple runs

<!-- end list -->

``` r
head(nulls$bcd)
```

    ## [1] 0.2199433 0.2199433 0.2199433 0.2199433 0.2199433 0.2199433

``` r
head(nulls$null_mod_seed)
```

    ## [1] 724874048 661397071 903248688   6143789 298299975 835417057

``` r
ggplot(nulls, aes(isd_turnover)) + geom_histogram() + geom_vline(xintercept = g_analyses$isd_turnover)
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](regional_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

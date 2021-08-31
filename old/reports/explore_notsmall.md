5 sites
================

  - [Time periods](#time-periods)
  - [Overlaps](#overlaps)
  - [State variables](#state-variables)

## Time periods

``` r
route_years <- select(all_overlaps, route, region, location.bcr, location.routename, startyears, endyears) %>%
  distinct() %>%
  mutate(start = as.integer(substr(startyears, 0, 4)),
         end = as.integer(substr(endyears, 0, 4)))


ggplot(route_years, aes(x = start, y = as.factor(route))) + 
  geom_errorbar(aes(xmin = start, xmax = end)) + xlab("Time coverage") + ylab("Route")
```

![](explore_notsmall_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

Note that 2, 5, and 16 are weirdos in that they start and end quite
early and are shorter than the others.

## Overlaps

``` r
all_overlaps <- left_join(all_overlaps, all_composition)
```

    ## Joining, by = c("route", "region", "location.bcr", "sim_seed")

``` r
ggplot(filter(all_overlaps, !is.na(sim_seed)), aes(overlap)) + geom_histogram() + facet_wrap(vars(route), scales = "free_y", ncol = 3) + geom_histogram(data = filter(all_overlaps, is.na(sim_seed)), fill = "orange") + geom_histogram(data = filter(all_overlaps, is.na(sim_seed)), aes(x = composition_overlap), fill = "green")
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](explore_notsmall_files/figure-gfm/unnamed-chunk-2-1.png)<!-- --> So…
given some amount of *species* turnover, there’s going to be some
constraint on how much the ISD can change. The ISD can’t change a lot
more than the species composition, but it can change a lot less.

If it changes a lot less, that could be because similarly sized species
are replacing each other.

There is however an additional constraint on how *little* it can change
given some amount of species turnover. Depending on the starting state
of the ISD and the species pool.

## State variables

``` r
all_sv_wide <- all_svs %>%
  select(timechunk, energy, biomass, abundance,route, region, sim_seed) %>%
  tidyr::pivot_wider(id_cols = c("route", "region", "sim_seed"), names_from = timechunk, values_from = c("energy", "biomass", "abundance"))

all_sv_change <- all_sv_wide %>%
  mutate(energy_lr = log(energy_end / energy_start),
         biomass_lr = log(biomass_end / biomass_start),
         abundance_lr = log(abundance_end/ abundance_start))

ggplot(filter(all_sv_change, is.na(sim_seed)), aes(energy_lr)) + 
  geom_histogram() +
  ggtitle("Real energy change (log ratio")
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](explore_notsmall_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
ggplot(filter(all_sv_change, is.na(sim_seed)), aes((abundance_lr))) + 
  geom_histogram() +
  ggtitle("Real abundance change (log ratio")
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](explore_notsmall_files/figure-gfm/unnamed-chunk-3-2.png)<!-- -->

``` r
ggplot(filter(all_sv_change, is.na(sim_seed)), aes(biomass_lr)) + 
  geom_histogram() +
  ggtitle("Real biomass change (log ratio")
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](explore_notsmall_files/figure-gfm/unnamed-chunk-3-3.png)<!-- -->

``` r
ggplot(all_sv_change, aes(abs(energy_lr), fill = is.na(sim_seed))) +
  geom_histogram() +
  facet_wrap(vars(route), scales= 'free')
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](explore_notsmall_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
only_real_change <- filter(all_sv_change, is.na(sim_seed))

ggplot(only_real_change, aes(abundance_lr, energy_lr)) + geom_point() + geom_abline(slope = 1, intercept= 0) #+ geom_point(data = filter(sv_change, is.na(sim_seed)), aes(abundance_change, energy_change), color = "pink")
```

![](explore_notsmall_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
only_real_smooths <- filter(all_smooths, is.na(sim_seed))


ggplot(only_real_smooths, aes(mass, start, group= route)) + geom_line()
```

![](explore_notsmall_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

<!-- ```{r} -->

<!-- start_overlaps <- list() -->

<!-- pairs <- expand.grid(a = unique(only_real_smooths$route), b = unique(only_real_smooths$route)) %>% -->

<!--   group_by_all() %>% -->

<!--   mutate(one = min(a, b), -->

<!--          two = max(a, b)) %>% -->

<!--   ungroup() %>% -->

<!--   filter(one != two) %>% -->

<!--   select(one, two) %>% -->

<!--   distinct() -->

<!-- for(i in 1:nrow(pairs)) { -->

<!--   comparison <- filter(only_real_smooths, route %in% pairs[i,]) %>% -->

<!--     select(route, mass, start, end) %>% -->

<!--     group_by_all() %>% -->

<!--     mutate(route = ifelse(route == pairs[i, 1], "one", "two")) %>% -->

<!--     ungroup() %>% -->

<!--     tidyr::pivot_wider(id_cols = mass, names_from = route, values_from = c("start", "end")) %>% -->

<!--     group_by_all() %>% -->

<!--     mutate(startmin = min(start_one, start_two), -->

<!--            endmin = min(end_one, end_two)) %>% -->

<!--     ungroup() -->

<!--   start_overlaps[[i]] <- data.frame( -->

<!--     one = pairs[i,1], -->

<!--     two = pairs[i, 2], -->

<!--     start_overlap = sum(comparison$startmin), -->

<!--     end_overlap = sum(comparison$endmin) -->

<!--   ) -->

<!-- } -->

<!-- ``` -->

<!-- ```{r} -->

<!-- simultaneous_overlaps <- bind_rows(start_overlaps) -->

<!-- ggplot(simultaneous_overlaps, aes(start_overlap)) + geom_histogram() -->

<!-- ggplot(simultaneous_overlaps, aes(end_overlap)) + geom_histogram() -->

<!-- ``` -->

``` r
only_real_smooths <- only_real_smooths %>%
  group_by(mass) %>%
  mutate(mean_diff = mean(end - start)) %>%
  ungroup()

ggplot(only_real_smooths, aes(mass, end - start, group = route)) + geom_segment(aes(x = mass, y = 0, xend = mass, yend = end - start), alpha = .1) + xlim(1, 8) + geom_line(aes(mass, mean_diff), color = "green")
```

    ## Warning: Removed 4896 rows containing missing values (geom_segment).

    ## Warning: Removed 4896 row(s) containing missing values (geom_path).

![](explore_notsmall_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
isd_r2 <- function(focal, compare) {
  
  focal_mean <- mean(focal)

  numer <- sum((focal - compare) ^ 2)
  denom <- sum((focal - focal_mean) ^ 2)
  1 - (numer/denom)
}


regional_r2s <- only_real_smooths %>%
  group_by(route) %>%
  mutate(diff_r2 = isd_r2(density_diff, mean_diff)) %>%
  ungroup()

ggplot(regional_r2s, aes(mass, density_diff, color = diff_r2, group = sim_seed)) + geom_line() + geom_line(aes(y = mean_diff), color = "green") + scale_color_viridis_c(option = "turbo", begin = .1, end = .9) + xlim(1.5, 7.5) + facet_wrap(vars(route))
```

    ## Warning: Removed 376 row(s) containing missing values (geom_path).
    
    ## Warning: Removed 376 row(s) containing missing values (geom_path).

![](explore_notsmall_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
ggplot(select(regional_r2s, route, diff_r2) %>% distinct(), aes(diff_r2)) + geom_histogram()
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](explore_notsmall_files/figure-gfm/unnamed-chunk-8-2.png)<!-- -->

``` r
regional_r2s %>% select(route, diff_r2) %>%
  summarize(mean_r2 = mean(diff_r2))
```

    ## # A tibble: 1 x 1
    ##   mean_r2
    ##     <dbl>
    ## 1   0.398

``` r
ggplot(filter(regional_r2s, route == 102), aes(mass, density_diff, color = diff_r2, group = sim_seed)) + geom_line() + geom_line(aes(y = mean_diff), color = "pink") + scale_color_viridis_c(option = "turbo", begin = .1, end = .9) + xlim(1.5, 7.5) + facet_wrap(vars(route))
```

    ## Warning: Removed 376 row(s) containing missing values (geom_path).
    
    ## Warning: Removed 376 row(s) containing missing values (geom_path).

![](explore_notsmall_files/figure-gfm/unnamed-chunk-8-3.png)<!-- -->

``` r
ggplot(filter(regional_r2s, route == 11), aes(mass, density_diff, color = diff_r2, group = sim_seed)) + geom_line() + geom_line(aes(y = mean_diff), color = "pink") + scale_color_viridis_c(option = "turbo", begin = .1, end = .9) + xlim(1.5, 7.5) + facet_wrap(vars(route))
```

    ## Warning: Removed 376 row(s) containing missing values (geom_path).
    
    ## Warning: Removed 376 row(s) containing missing values (geom_path).

![](explore_notsmall_files/figure-gfm/unnamed-chunk-8-4.png)<!-- -->

``` r
manual_r2 <- only_real_smooths %>%
  mutate(residual = density_diff - mean_diff) %>%
  mutate(squared_resid = residual ^ 2) %>%
  group_by(route) %>%
  mutate(route_mean = mean(density_diff)) %>%
  mutate(route_dist_from_mean = density_diff - route_mean) %>%
  mutate(route_dist_squared = route_dist_from_mean ^ 2) %>%
  summarize(residuals_sum_squared = sum(squared_resid),
            obs_sum_squared = sum(route_dist_squared)) %>%
  ungroup() %>%
  mutate(man_r2 = 1 - (residuals_sum_squared / obs_sum_squared))
```

Sometimes the R2 is really bad (negative).

``` r
isd_r2(only_real_smooths$density_diff, only_real_smooths$mean_diff)
```

    ## [1] 0.5574226

``` r
ggplot(only_real_smooths, aes(mean_diff, density_diff)) + 
  geom_point(alpha = .1) +
  geom_abline(slope = 1, intercept = 0) 
```

![](explore_notsmall_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
library(gratia)
library(mgcv)
```

    ## Loading required package: nlme

    ## 
    ## Attaching package: 'nlme'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     collapse

    ## This is mgcv 1.8-33. For overview type 'help("mgcv-package")'.

``` r
regional_gam <- gam(density_diff ~ s(mass, k = 25), data = only_real_smooths)

gam_fit <- add_fitted(only_real_smooths, regional_gam)

ggplot(gam_fit, aes(mass, .value)) + geom_line() + geom_line(aes(y = mean_diff), color = "pink") + geom_line(aes(y = density_diff, group = route), alpha = .1)
```

![](explore_notsmall_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
summary(regional_gam)
```

    ## 
    ## Family: gaussian 
    ## Link function: identity 
    ## 
    ## Formula:
    ## density_diff ~ s(mass, k = 25)
    ## 
    ## Parametric coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)
    ## (Intercept) 1.533e-18  6.551e-06       0        1
    ## 
    ## Approximate significance of smooth terms:
    ##           edf Ref.df     F p-value    
    ## s(mass) 23.53  23.97 322.3  <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## R-sq.(adj) =    0.3   Deviance explained = 30.1%
    ## GCV = 7.7344e-07  Scale est. = 7.7238e-07  n = 18000

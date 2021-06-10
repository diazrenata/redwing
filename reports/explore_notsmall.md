5 sites
================

  - [Overlaps](#overlaps)

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

![](explore_notsmall_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

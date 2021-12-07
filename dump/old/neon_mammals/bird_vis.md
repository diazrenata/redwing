Comparisons
================

``` r
allCompsObs <- filter(allComps, sim  < 0)

allCompsNull <- filter(allComps, sim > 0)

allCompsCompare <- allCompsNull %>%
  left_join(
    select(
    rename(
      allCompsObs, obs_isd_overlap = isd_overlap
    ),
    -sim, -shuffle_seed
    )
  )
```

    ## Joining, by = c("species_overlap", "bcd", "haver", "timeperiod", "years", "route.x", "region.x", "location.bcr.x", "location.longitude.x", "location.latitude.x", "timeperiod.x", "years.x", "route.y", "region.y", "location.bcr.y", "location.longitude.y", "location.latitude.y", "timeperiod.y", "years.y", "site.x", "site.y", "location.routename.x", "location.routename.y", "same_state", "same_bcr")

``` r
allCompsPerc <- allCompsCompare %>%
  group_by(
    site.x, site.y
  ) %>%
  mutate(nlower = sum(isd_overlap < obs_isd_overlap),
         nlowerinc = sum(isd_overlap <= obs_isd_overlap), # there are no ties so far
         nsims = length(unique(sim))) %>%
  mutate(percentile = nlower / nsims) %>%
  ungroup() %>%
  select(-isd_overlap, -sim, -shuffle_seed) %>%
  distinct()

ggplot(allCompsPerc, aes(percentile)) +
  geom_histogram(bins = 100) +
  geom_vline(xintercept = c(.95)) 
```

![](bird_vis_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

``` r
ggplot(allCompsPerc, aes(percentile)) +
  geom_histogram() +
  geom_vline(xintercept = c(.95)) +
  facet_wrap(vars(same_bcr), scales = "free_y")
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](bird_vis_files/figure-gfm/unnamed-chunk-1-2.png)<!-- -->

``` r
ggplot(allCompsPerc, aes(haver, percentile, color = same_bcr)) +
  geom_point() 
```

![](bird_vis_files/figure-gfm/unnamed-chunk-1-3.png)<!-- -->

``` r
ggplot(allCompsObs, aes(species_overlap, isd_overlap)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  ylim(0,1) +
  xlim(0,1)
```

![](bird_vis_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
ggplot(allCompsPerc, aes(species_overlap, obs_isd_overlap, color = percentile < .025)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  ylim(0,1) +
  xlim(0,1)
```

![](bird_vis_files/figure-gfm/unnamed-chunk-2-2.png)<!-- -->

``` r
ggplot(allCompsObs, aes(haver, isd_overlap)) +
  geom_point() 
```

![](bird_vis_files/figure-gfm/unnamed-chunk-2-3.png)<!-- -->

``` r
ggplot(allCompsObs, aes(haver, species_overlap)) +
  geom_point()
```

![](bird_vis_files/figure-gfm/unnamed-chunk-2-4.png)<!-- -->

``` r
ggplot(allCompsObs, aes(haver, bcd)) +
  geom_point()
```

![](bird_vis_files/figure-gfm/unnamed-chunk-2-5.png)<!-- -->

``` r
ggplot(allCompsObs, aes(species_overlap)) +
  geom_histogram()
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](bird_vis_files/figure-gfm/unnamed-chunk-2-6.png)<!-- -->

``` r
ggplot(allCompsObs, aes(isd_overlap)) +
  geom_histogram()
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](bird_vis_files/figure-gfm/unnamed-chunk-2-7.png)<!-- -->

``` r
allCompsNullMeans <- allCompsNull %>%
  group_by(route.x, region.x, route.y, region.y) %>%
  summarize(mean_isd_overlap = mean(isd_overlap),
            sd_isd_overlap = sd(isd_overlap)) %>%
  ungroup() %>%
  left_join(((allCompsObs))) %>%
  mutate(ses = (isd_overlap - mean_isd_overlap) / sd_isd_overlap)
```

    ## `summarise()` has grouped output by 'route.x', 'region.x', 'route.y'. You can override using the `.groups` argument.

    ## Joining, by = c("route.x", "region.x", "route.y", "region.y")

``` r
ggplot(allCompsNullMeans, aes(species_overlap, mean_isd_overlap)) +
  geom_point(alpha = .2) +
  geom_point(data = allCompsObs, aes(species_overlap, isd_overlap), color = "blue", alpha = .1)
```

![](bird_vis_files/figure-gfm/unnamed-chunk-2-8.png)<!-- -->

``` r
ggplot(allCompsNullMeans, aes(ses)) +
  geom_histogram() +
  geom_vline(xintercept = c(-1.96, 1.96))
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](bird_vis_files/figure-gfm/unnamed-chunk-2-9.png)<!-- -->

``` r
ggplot(allCompsNullMeans, aes(haver, ses, color = abs(ses) > 1.96)) + geom_point()
```

![](bird_vis_files/figure-gfm/unnamed-chunk-2-10.png)<!-- -->

``` r
ggplot(allCompsNullMeans, aes(haver, ses, color = (ses) < -1.96)) + geom_point()
```

![](bird_vis_files/figure-gfm/unnamed-chunk-2-11.png)<!-- -->

``` r
mean(abs(allCompsNullMeans$ses) > 1.96)
```

    ## [1] 0.06169556

``` r
mean(allCompsNullMeans$ses < -1.96)
```

    ## [1] 0.03496082

``` r
mean(allCompsNullMeans$ses > 1.96)
```

    ## [1] 0.02673474

``` r
mean(allCompsPerc$percentile < 0.025)
```

    ## [1] 0.05290217

Birds are bang-on random.

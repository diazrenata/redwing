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

    ## Joining, by = c("site.x", "site.y", "species_overlap", "bcd", "decimalLatitude.x", "decimalLongitude.x", "decimalLatitude.y", "decimalLongitude.y", "haver", "domainID.x", "domainID.y", "same_domain")

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
  geom_histogram() +
  geom_vline(xintercept = c(.95)) 
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](vis_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

``` r
ggplot(allCompsPerc, aes(percentile)) +
  geom_histogram() +
  geom_vline(xintercept = c(.95)) +
  facet_wrap(vars(same_domain), scales = "free_y")
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](vis_files/figure-gfm/unnamed-chunk-1-2.png)<!-- -->

``` r
ggplot(allCompsPerc, aes(haver, percentile, color = same_domain)) +
  geom_point() 
```

![](vis_files/figure-gfm/unnamed-chunk-1-3.png)<!-- -->

``` r
ggplot(allCompsObs, aes(species_overlap, isd_overlap)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  ylim(0,1) +
  xlim(0,1)
```

![](vis_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
ggplot(allCompsPerc, aes(species_overlap, obs_isd_overlap, color = percentile > .95)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  ylim(0,1) +
  xlim(0,1)
```

![](vis_files/figure-gfm/unnamed-chunk-2-2.png)<!-- -->

``` r
ggplot(allCompsObs, aes(haver, isd_overlap)) +
  geom_point() 
```

![](vis_files/figure-gfm/unnamed-chunk-2-3.png)<!-- -->

``` r
ggplot(allCompsObs, aes(haver, species_overlap)) +
  geom_point()
```

![](vis_files/figure-gfm/unnamed-chunk-2-4.png)<!-- -->

``` r
ggplot(allCompsObs, aes(haver, bcd)) +
  geom_point()
```

![](vis_files/figure-gfm/unnamed-chunk-2-5.png)<!-- -->

``` r
allCompsConserved <- filter(allCompsPerc, percentile > .95)
```

ISDs overlap too much | species overlap about 20% of the time (compared
to 5% at random). No obvious relationship with distance (great-circle)
or domain-domain (but very few within-domain comparisons).

Maybe the similarity of sites along other axes? Maybe distance isn’t
nuanced enough, maybe other environmental covariates.

A little creeped out by possible species richness effects, etc. Not sure
yet on the intuition there.

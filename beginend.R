get_begin_end <- function(dataset) {

  startyears <- dataset$covariates$year[1:5]

  endyears <- dataset$covariates$year[(nrow(dataset$covariates)-4) : nrow(dataset$covariates)]


isd1 <- simulate_isd_ts(dataset, censusyears = startyears)
isd2 <- simulate_isd_ts(dataset, censusyears = endyears)
isd1_kde <- add_kde(isd1$isd) %>%
  mutate(chunk = "start")
isd2_kde <- add_kde(isd2$isd) %>%
  mutate(chunk = "end")

both_isds <- dplyr::bind_rows(isd1_kde, isd2_kde) %>%
  tidyr::pivot_wider(id_cols = mass, names_from = chunk, values_from = density) %>%
  mutate(density_diff = end - start,
         density_rat = end / start)

both_isds_with_id <- as.data.frame(dataset$metadata) %>%
  select(route, region) %>%
  distinct() %>%
  cbind(both_isds)
return(both_isds_with_id)

}

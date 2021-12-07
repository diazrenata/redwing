

fit_gmm <- function(size_vect) {
  library(mclust)
  this_gmm <- densityMclust(size_vect, G = c(1:15) )

  return(this_gmm)

}

add_gmm <- function(isd) {

  isd <- isd %>%
    mutate(logmass = log(mass))

  gmm <- fit_gmm(isd$logmass)


  gmm_isd <- data.frame(logmass = seq(0, log(15000), length.out = 1000))
  gmm_isd$dens <- predict(gmm, newdata = gmm_isd$logmass)


  isd_gmm <- data.frame(
    mass = gmm_isd$logmass,
    density = (gmm_isd$dens)/ sum(gmm_isd$dens)
  )

  return(isd_gmm)

}

get_begin_end_gmm <- function(dataset) {

  startyears <- dataset$covariates$year[1:5]

  endyears <- dataset$covariates$year[(nrow(dataset$covariates)-4) : nrow(dataset$covariates)]


  isd1 <- simulate_isd_ts(dataset, censusyears = startyears)
  isd2 <- simulate_isd_ts(dataset, censusyears = endyears)
  isd1_gmm <- add_gmm(isd1$isd) %>%
    dplyr::mutate(chunk = "start")
  isd2_gmm <- add_gmm(isd2$isd) %>%
    dplyr::mutate(chunk = "end")

  both_isds <- dplyr::bind_rows(isd1_gmm, isd2_gmm) %>%
    tidyr::pivot_wider(id_cols = mass, names_from = chunk, values_from = density) %>%
    mutate(density_diff = end - start,
           density_rat = end / start)

  both_isds_with_id <- as.data.frame(dataset$metadata) %>%
    select(route, region, location.bcr, location.regionname, location.routename) %>%
    distinct() %>%
    cbind(both_isds)

  return(both_isds_with_id)

}

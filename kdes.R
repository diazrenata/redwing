fit_kde <- function(size_vect) {


  this_kde <- density(size_vect, n = 1000, from = 0, to = log(15000))

  return(this_kde)

}

add_kde <- function(isd) {

  isd <- isd %>%
    mutate(logmass = log(mass))

  kd <- fit_kde(isd$logmass)

  isd_kde <- data.frame(
    mass = kd$x,
    density = (kd$y)/ sum(kd$y)
  )

  return(isd_kde)

}

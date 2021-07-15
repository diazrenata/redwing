
shuffle_neon_species <- function(all_rats_counts, shuffle_number = NA, shuffle_seed = NULL) {

  splist <- select(all_rats_counts, scientificName) %>%
    distinct()

  if(is.null(shuffle_seed)) {
    shuffle_seed = sample.int(1000000000, size = 1)
  }

  set.seed(shuffle_seed)

  newNames <- sample(splist$scientificName, size = nrow(splist), replace = F)

  splist <- splist %>%
    rename(oldName = scientificName) %>%
    mutate(scientificName = newNames,
           sim = shuffle_number,
           shuffle_seed = shuffle_seed)

  return(splist)
}

change_neon_species <- function(all_rats_counts, shuffled_splist) {

  all_rats_counts_shuffled <- all_rats_counts %>%
    select(-sim) %>%
    mutate(oldName = scientificName) %>%
    select(-scientificName) %>%
    left_join(shuffled_splist)

  return(all_rats_counts_shuffled)

}

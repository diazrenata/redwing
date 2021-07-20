make_bird_pairs <- function(all_datasets) {

  dataset_info <- select(all_datasets,
                         matssname,
                         route,
                         region,
                         location.bcr,
                         location.routename)

  site_pairs <- as.data.frame(
    expand.grid(
      site1 = all_datasets$matssname,
      site2 = all_datasets$matssname
    )
  ) %>%
    mutate(site1 = as.character(site1),
           site2 = as.character(site2)) %>%
    filter(site1 != site2) %>%
    group_by_all() %>%
    mutate(sitealpha1 = min(site1, site2),
           sitealpha2 = max(site1, site2)) %>%
    ungroup() %>%
    select(sitealpha1, sitealpha2) %>%
    distinct() %>%
    rename(site.x = sitealpha1,
           site.y = sitealpha2) %>%
    left_join(dataset_info, by = c("site.x" = "matssname")) %>%
    left_join(dataset_info, by = c("site.y"  = "matssname")) %>%
    group_by_all() %>%
    mutate(same_state = region.x == region.y,
           same_bcr = location.bcr.x == location.bcr.y) %>%
    ungroup()

  return(site_pairs)

}

pull_bird_dat <- function(matss_dataset, years = c(2014:2018), timedesc = NA) {

  yearRows <- which(matss_dataset$covariates$year %in% years)

  new_dataset <- matss_dataset

  new_dataset$abundance <- matss_dataset$abundance[yearRows,]
  new_dataset$covariates <- matss_dataset$covariates[ yearRows, ]
  new_dataset$metadata$years = years
  new_dataset$metadata$timeperiod = timedesc

  these_species <- colnames(new_dataset$abundance)
  nonzero_species <- which(colSums(new_dataset$abundance) > 0)

  these_species <- these_species[ nonzero_species]

  new_dataset$abundance <- new_dataset$abundance[ , nonzero_species]

  new_dataset$metadata$species_table <- new_dataset$metadata$species_table %>%
    filter(id %in% these_species)


  return(new_dataset)

}

pull_bird_sp <- function(matss_dataset) {

  splist <- select(matss_dataset$metadata$species_table, id)

  return(splist)

}

get_bird_counts <- function(matss_dataset) {

abund_counts <- matss_dataset$abundance %>%
  tidyr::pivot_longer(everything(),names_to = "id", values_to = "abundance") %>%
  group_by(id) %>%
  summarize(abundance = sum(abundance)) %>%
  ungroup() %>%
  mutate(sim = -99,
         oldID = id,
         route = matss_dataset$metadata$route,
         region = matss_dataset$metadata$region,
         location.bcr = matss_dataset$metadata$location$bcr,
         location.longitude = matss_dataset$metadata$location$longitude,
         location.latitude = matss_dataset$metadata$location$latitude,
         timeperiod = matss_dataset$metadata$timeperiod,
         years = toString(matss_dataset$metadata$years))

return(abund_counts)


}

shuffle_bird_species <- function(all_bird_spp, shuffle_number = NA, shuffle_seed = NULL) {


  if(is.null(shuffle_seed)) {
    shuffle_seed = sample.int(1000000000, size = 1)
  }

  set.seed(shuffle_seed)

  newNames <- sample(all_bird_spp$id, size = nrow(all_bird_spp), replace = F)

  all_bird_spp <- all_bird_spp %>%
    rename(oldID = id) %>%
    mutate(id = newNames,
           sim = shuffle_number,
           shuffle_seed = shuffle_seed)

  return(all_bird_spp)

}

change_bird_species <- function(bird_counts, bird_shuffled_splist) {


  bird_counts_shuffled <- bird_counts %>%
    select(-sim) %>%
    select(-id) %>%
    left_join(bird_shuffled_splist)

  return(bird_counts_shuffled)

}

describe_bird_counts <- function(bird_counts, sd_table = BBSsize::sd_table, shuffled_sp = NULL, all_bird_spp) {

  if(!is.null(shuffled_sp)) {
    bird_counts <- change_bird_species(bird_counts, shuffled_sp)
  }

  bird_counts_nooldID <- bird_counts %>%
    select(-oldID)

  thisISD = BBSsize::simulate_isd(bird_counts_nooldID, sd_table)
  thisGMM = rwar::add_gmm(thisISD)


  counts = all_bird_spp %>%
    select(id) %>%
    distinct() %>%
    left_join(select(bird_counts, id, abundance)) %>%
    group_by_all() %>%
    mutate(abundance = ifelse(is.na(abundance), 0, abundance)) %>%
    ungroup()

  thisAllRel <- counts %>%
    mutate(totalAbund = sum(abundance)) %>%
    mutate(relAbund = abundance / totalAbund)  %>%
    select(id, relAbund)

  thisAllCounts <- counts %>%
    tidyr::pivot_wider(names_from = id, values_from = abundance)

  thisMeta <- bird_counts  %>%
    select(-route, -region, -id, -oldID, -abundance, -location.bcr, -location.longitude, -location.latitude) %>%
    distinct()

  thisSite <- bird_counts %>%
    select(route, region, location.bcr, location.longitude, location.latitude, timeperiod, years) %>%
    distinct()

  siteID = paste0("bbs_rtrg_", thisSite$route, "_", thisSite$region)

  return(list(
    siteID = siteID,
    isd = thisISD,
    gmm = thisGMM,
    allCounts = thisAllCounts,
    allRel = thisAllRel,
    metaInfo = thisMeta,
    siteInfo = thisSite
  ))
}


compare_bird_pairs <- function(birdList.x, birdList.y) {

  gmm.x = birdList.x$gmm
  gmm.y = birdList.y$gmm

  pair_isds <- left_join(gmm.x, gmm.y, by = "mass")

  ol <- pair_isds %>%
    group_by_all() %>%
    mutate(mindensity = min(density.x, density.y)) %>%
    ungroup()

  isd_overlap <- sum(ol$mindensity)

  rel.x = birdList.x$allRel
  rel.y = birdList.y$allRel

  pair_spp <- left_join(rel.x, rel.y, by = "id")

  sp_ol <- pair_spp %>%
    group_by_all() %>%
    mutate(minRelAbund = min(relAbund.x, relAbund.y)) %>%
    ungroup()

  species_overlap = sum(sp_ol$minRelAbund)

  counts_matrix <- bind_rows(
    birdList.x$allCounts,
    birdList.y$allCounts
  ) %>%
    as.matrix()

  bcd = vegan::vegdist(counts_matrix, "bray")[[1]]

  haver = geosphere::distHaversine(p1 = c(birdList.x$siteInfo$location.longitude, birdList.x$siteInfo$location.latitude), p2 = c(birdList.y$siteInfo$location.longitude, birdList.y$siteInfo$location.latitude))

  add_x <- function(cn) {
    return(paste0(cn, ".x"))
  }
  add_y <- function(cn) {
    return(paste0(cn, ".y"))
  }



  colnames(birdList.x$siteInfo) <- add_x(colnames(birdList.x$siteInfo))
  colnames(birdList.y$siteInfo) <- add_y(colnames(birdList.y$siteInfo))

  return(data.frame(
    isd_overlap = isd_overlap,
    species_overlap = species_overlap,
    bcd = bcd,
    haver = haver
  ) %>%
    bind_cols(birdList.x$metaInfo) %>%
    bind_cols(birdList.x$siteInfo) %>%
    bind_cols(birdList.y$siteInfo)
  )

}

remove_transients <- function(matss_dataset, transient_threshold = 2/3) {

  abundance_binary <- matss_dataset$abundance > 0

  nobs <- colSums(abundance_binary)

  prop_obs <- nobs / nrow(abundance_binary)

  not_transient_spp <- which(prop_obs > transient_threshold)
  not_transient_spp_names <- names(prop_obs)[which(prop_obs > transient_threshold)]


  not_abundance <- matss_dataset$abundance[ ,not_transient_spp]
  not_metadata <- matss_dataset$metadata

  not_metadata$species_table <- not_metadata$species_table %>%
    dplyr::filter(id %in% not_transient_spp_names)

  not_dataset <- list(
    abundance = not_abundance,
    covariates = matss_dataset$covariates,
    metadata = not_metadata
  )

  return(not_dataset)
}

make_all_bird_comparisons <- function(pairs_df, siteDescriptions) {

  siteIDS <- lapply(siteDescriptions, FUN = function(list) return(list$siteID))
  siteIDS <- as.vector(siteIDS)
  names(siteDescriptions) <- siteIDS

  all_comparisons_real <- list()

  for(i in 1:nrow(pairs_df)) {
    all_comparisons_real[[i]] <- compare_bird_pairs(siteDescriptions[[pairs_df$site.x[i]]], siteDescriptions[[pairs_df$site.y[i]]])
  }

  all_comp_df <- bind_rows(all_comparisons_real) %>%
    left_join(pairs_df)

  return(all_comp_df)
}

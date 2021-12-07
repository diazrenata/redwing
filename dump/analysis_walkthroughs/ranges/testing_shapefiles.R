library(sf)
library(dplyr)

allsp <- st_read("analysis_walkthroughs/ranges/species_in_bbs/species_in_bbs/species_in_bbs.shp")

resident_or_breeding <- filter(allsp, presence == 1, seasonal %in% c(1, 2))

resident <- filter(allsp, presence == 1, seasonal == 1)
breeding <- filter(allsp, presence == 1, seasonal == 2)

present <- allsp %>% filter(presence == 1)


length(unique(resident_or_breeding$binomial))

tal <- resident %>% as.data.frame() %>% group_by(binomial) %>% summarize(n = dplyr::n())

bbs_sp <- BBSsize::sd_table

bl_sp <- unique(resident_or_breeding$binomial)

bbs_binomial <- paste0(bbs_sp$genus, " ", bbs_sp$species)


setdiff(bl_sp, bbs_binomial)
setdiff(bbs_binomial, bl_sp)

# some species have multiple shape files, I think reflecting different values for "origin". Going to go with present if ANY of the associated shapefiles overlap route.


# there are 804 entries in resident_or_breeding. we want to get the intersection of these with a database of ~500 routes.

library(dplyr)
library(ggplot2)
library(BBSsize)
library(rwar)

h <- get_portal_rats()

hstart <- h$abundance[1:5,]
hend <- h$abundance[27:31,]

htime <- data.frame(
  species = colnames(hstart),
  start = colSums(hstart),
  end = colSums(hend)
)

htime <- htime %>%
  mutate(startrel = start / sum(start),
         endrel = end / sum(end))

ggplot(htime, aes(species, endrel - startrel)) + geom_col()

htime <- htime %>%
  arrange(desc(endrel - startrel))

hsize <- get_portal_sd_dat() %>%
  select(id, mean_mass) %>%
  rename(species = id) %>%
  filter(species %in% htime$species) %>%
  arrange(desc(mean_mass)) %>%
  mutate(size_rank = row_number())


htoy <- htime %>%
  mutate(win_rank = row_number()) %>%
  select(win_rank, species) %>%
  rename(orig_species = species) %>%
  left_join(select(hsize, size_rank, mean_mass, species), by = c("win_rank" = "size_rank"))

toyabund <- h$abundance[,htoy$orig_species]
colnames(toyabund) <- htoy$species

toyh <- h

toyh$abundance <- toyabund

be <- get_begin_end_isds(toyh, sd_dat = get_portal_sd_dat())
sths <- get_begin_end_smooths(be)
ol <- overlap(sths)
comp <- get_begin_end_composition(toyh)
svc <- get_begin_end_svs(be)

ggplot(sths, aes(mass, start)) + geom_line() + geom_line(aes(mass, end), color = "blue")

library(dplyr)
library(BBSsize)
library(drake)
library(MATSS)
source(here::here("kdes.R"))
source(here::here("beginend.R"))
source(here::here("gmms.R"))
datasets <- MATSS::build_bbs_datasets_plan()

methods <- drake_plan(
  beginend = target(get_begin_end(dataset),
                    transform = map(
                      dataset = !!rlang::syms(datasets$target))),
  gmm_beginend = target(get_begin_end_gmm(dataset),
                        transform = map(
                          dataset = !!rlang::syms(datasets$target))),
  all_beginend = target(dplyr::bind_rows(beginend),
                        transform = combine(beginend)),
  all_gmm_beginend = target(dplyr::bind_rows(gmm_beginend),
                            transform = combine(gmm_beginend))
)

all = bind_rows(datasets, methods)

## Set up the cache and config
db <- DBI::dbConnect(RSQLite::SQLite(), here::here("drake-cache.sqlite"))
cache <- storr::storr_dbi("datatable", "keystable", db)
cache$del(key = "lock", namespace = "session")


# Run the pipeline on multiple local cores
system.time(make(all, cache = cache, cache_log_file = here::here("cache_log_bbs.txt"), verbose = 1, memory_strategy = "autoclean"))

loadd(all_gmm_beginend, cache = cache)

all_beginend <- all_beginend %>%
  mutate(rtrg = paste0(route, "_", region))

all_gmm_beginend <- all_gmm_beginend %>%
  mutate(rtrg = paste0(route, "_", region))


library(ggplot2)

ggplot(all_gmm_beginend, aes(mass, density_diff, group = rtrg)) + geom_line(alpha = .1)

all_beginend_summ <- all_gmm_beginend %>%
  group_by(mass) %>%
  summarize(meandiff = mean(density_diff)) %>%
  ungroup()

some_routes <- sample(unique(all_gmm_beginend$rtrg), size = 1000)

ggplot(filter(all_gmm_beginend, rtrg %in% some_routes), aes(mass, density_diff, group = rtrg)) + geom_line(alpha = .1) + geom_line(data = all_beginend_summ, aes(mass, meandiff), inherit.aes = F, color = "green")

ggplot(all_beginend_summ, aes(mass,  meandiff)) + geom_line()


ct <- filter(all_gmm_beginend, region == 18) %>%
  group_by(mass) %>%
  mutate(meandiff = mean(density_diff)) %>%
  ungroup()

ggplot(filter(ct), aes(mass, density_diff, group = rtrg)) + geom_line(alpha = .3) + geom_line(aes(y = meandiff), color = "purple") + xlim(4,5.5)



ggplot(filter(ct), aes(mass, end, group = rtrg)) + geom_line(alpha = .3) + geom_line(aes(y = meandiff), color = "purple") + xlim(4,5.5)

ggplot(ct, aes(mass, start, group = rtrg)) + geom_line(alpha = .4) + geom_line(aes(y = end), alpha = .4, color = "blue") + geom_line(aes(y = meandiff), color = "purple")

ggplot(filter(ct, route == 116), aes(meandiff, density_diff)) + geom_point() + geom_abline(slope = 1, intercept = 0)

ct %>% select(mass, meandiff) %>% distinct() %>% summarize(abssum = sum(abs(meandiff)))

ct %>% group_by(route) %>% summarize(route_abbsum = sum(abs(density_diff))) %>% ungroup() %>% summarize(overall_mean_abssum = mean(route_abbsum))

all_beginend_summ %>% select(mass, meandiff) %>% distinct() %>% summarize(abssum = sum(abs(meandiff)))

all_gmm_beginend %>% group_by(route, region) %>% summarize(route_abbsum = sum(abs(density_diff))) %>% ungroup() %>% summarize(overall_mean_abssum = mean(route_abbsum))

library(dplyr)
load(here::here("aspirational_structure", "dev_vignettes", "hbrm1.Rds"))
newdat <- hbrm1$data %>%
  filter(matssname %in% unique(hbrm1$data$matssname)[1:5])

library(brms)
hbrm_pred <- newdat %>% mutate(predicted = predict(hbrm1, newdata = newdat)[,1])
save(hbrm_pred, file = here::here("aspirational_structure", "dev_vignettes", "hbrm1_pred.Rds"))

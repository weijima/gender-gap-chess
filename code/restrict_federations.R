library(tidyverse)

rating_data <- read_rds("../interactive/precalculated/data_processed.rds")

feds <- rating_data %>%
  filter(min_players == 30, include_inactive == TRUE, birth_uncertain == FALSE) %>%
  filter(max_byear >= 1999) %>%
  group_by(max_byear, min_rating) %>%
  summarise(feds = list(unique(fed))) %>%
  ungroup() %>%
  mutate(nfeds = map_int(feds, length)) %>%
  filter(nfeds == min(nfeds)) %>%
  pull(feds) %>%
  pluck(1)


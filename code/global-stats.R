library(tidyverse)

participation_gap <- function(rating_data) {
  rating_data %>%
    count(fed, sex, name = "no_of_players") %>%
    pivot_wider(names_from = "sex", values_from = "no_of_players", values_fill = 0) %>%
    mutate(participation_gap = `M` / (`F` + `M`))
}

federations <- function(rating_data, min_players) {
  participation_gap(rating_data) %>%
    mutate(no_minority = pmin(`F`, `M`)) %>%
    filter(no_minority >= min_players) %>%
    pull(fed)
}

restrict_data <- function(rating_data, include_junior, include_inactive, min_rating) {
  if (include_junior) max_byear <- 2019 else max_byear <- 1999
  rating_data %>%
    filter(if (include_inactive) TRUE else active) %>%
    filter(!is.na(born)) %>%
    filter(born <= max_byear | is.na(born), rating >= min_rating)
}


rating_data <- read_csv("data/rating-data.csv", col_types = "cccdiil")

global_data <-
  crossing(juniors = c(TRUE, FALSE),
           inactives = c(TRUE, FALSE),
           floor = c(1000, 1400, 1600)) %>%
  mutate(male = pmap(., ~ {
    rating_data %>%
      restrict_data(include_junior = ..1, include_inactive = ..2, min_rating = ..3) %>%
      filter(sex == "M") %>%
      pull(rating)
  } )) %>%
  mutate(female = pmap(., ~ {
    rating_data %>%
      restrict_data(include_junior = ..1, include_inactive = ..2, min_rating = ..3) %>%
      filter(sex == "F") %>%
      pull(rating)
  } )) %>%
  mutate(MW = map2(male, female, wilcox.test, conf.int = TRUE)) %>%
  mutate(KS = map2(male, female, ks.test))


global_data %>%
  mutate(MW = map(MW, broom::tidy),
         KS = map(KS, broom::tidy)) %>%
  unnest(c(MW, KS), names_sep = "_") %>%
  select(-contains("male"), -contains("method"), -contains("alternative")) %>%
  mutate(MW_statistic = as.character(MW_statistic),
         across(matches("MW_(e|c)"), round)) %>%
  write_csv("data/global-stat-data.csv")

# read_csv("data/global-stat-data.csv", col_types = "lliicdii")

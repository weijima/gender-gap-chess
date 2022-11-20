library(tidyverse)
library(Rcpp)

clargs <- commandArgs(trailingOnly = TRUE)
if (length(clargs) > 0) {
  imin <- as.integer(clargs[1])
  imax <- as.integer(clargs[2])
} else {
  imin <- 1
  imax <- 5
}

sourceCpp("permutation_tests.cpp")

federations <- function(rating_data, min_players) {
  rating_data %>%
    count(fed, sex, name = "no_of_players") %>%
    pivot_wider(names_from = "sex", values_from = "no_of_players") %>%
    replace_na(list(`F` = 0, `M` = 0)) %>%
    mutate(participation_gap = `M` / (`F` + `M`)) %>%
    mutate(no_minority = pmin(`F`, `M`)) %>%
    filter(no_minority >= min_players) %>%
    pull(fed)
}


rating_data <- read_rds("../../data/rating_data.rds")

fact <- crossing(
  max_byear = seq(1987, 2014, by = 3),
  min_rating = seq(1100, 1600, by = 50),
  min_players = seq(20, 40, by = 10),
  include_inactive = c(FALSE, TRUE),
  birth_uncertain = c(FALSE, TRUE)
)

for (i in imin:imax) {
  rating_data %>%
    filter(if (fact$include_inactive[i]) TRUE else active) %>%
    filter(if (fact$birth_uncertain[i]) TRUE else born != 0) %>%
    filter(born <= fact$max_byear[i], rating >= fact$min_rating[i]) %>%
    filter(fed %in% federations(., fact$min_players[i])) %>%
    select(fed, sex, rating) %>%
    pivot_wider(names_from = sex, values_from = rating, values_fn = list) %>%
    mutate(stats = map2(`F`, `M`, permtest, perms = 100000)) %>%
    unnest(stats) %>%
    mutate(participation_gap = map2_dbl(F, M, ~length(.y)/(length(.x)+length(.y)))) %>%
    select(-c(`F`, `M`)) %>%
    mutate(across(c(participation_gap, diff), ~round(.x, 3))) %>%
    bind_cols(fact %>% slice(i)) %>%
    relocate(colnames(fact), .before = 1) %>%
    write_csv(str_c("tmp/results_", sprintf("%04d", i), ".csv"))
}

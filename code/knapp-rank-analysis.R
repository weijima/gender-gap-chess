library(tidyverse)
library(extraDistr)


restrict_data <- function(rating_data, juniors, inactives, floor,
                          birth_uncertain = FALSE) {
  if (juniors) max_byear <- 2019 else max_byear <- 1999
  rating_data %>%
    filter(if (inactives) TRUE else active) %>%
    filter(if (birth_uncertain) TRUE else !is.na(born)) %>%
    filter(born <= max_byear | is.na(born), rating >= floor)
}


top_Kth_woman_rank <- function(rating_tab, K = 1) {
  rating_tab %>%
    arrange(desc(rating)) %>%
    rowid_to_column("rank") %>%
    filter(sex == "F") %>%
    pull(rank) %>%
    pluck(K)
}

prob_rank_random <- function(rating_tab, K = 1) {
  observed_rank <- top_Kth_woman_rank(rating_tab, K)
  num_women <- nrow(filter(rating_tab, sex == "F"))
  num_men <- nrow(filter(rating_tab, sex == "M"))
  ranks <- observed_rank:(observed_rank + num_men)
  1 - pnhyper(observed_rank - 1, num_men, num_women, K)
}



rating_data <- read_csv("data/rating-data.csv", col_types = "ccciiil")

null_stats <- read_csv("data/null-stats.csv", col_types = "llicccd")



# Global analysis
global_anal <-
  crossing(
    juniors = c(FALSE, TRUE),
    inactives = c(FALSE, TRUE),
    floor = c(1000L, 1400L, 1600L),
    K = c(1L, 10L)
  ) %>%
  mutate(
    raw_pval = pmap_dbl(
      list(juniors, inactives, floor, K),
      \(juniors, inactives, floor, K) {
        rating_data %>%
          restrict_data(juniors, inactives, floor) %>%
          prob_rank_random(K)
      },
      .progress = TRUE
    )
  )

global_anal %>%
  mutate(pval = 2 * pmin(raw_pval, 1 - raw_pval)) %>%
  arrange(K) %>%
  print(n = Inf)



# Per-federation analysis
per_fed_anal <-
  null_stats %>%
  distinct(juniors, inactives, floor, fed) %>%
  filter(fed != "ALL") %>%
  crossing(K = c(1L, 10L)) %>%
  mutate(
    raw_pval = pmap_dbl(
      list(juniors, inactives, floor, fed, K),
      \(juniors, inactives, floor, f, K) {
        rating_data %>%
          restrict_data(juniors, inactives, floor) %>%
          filter(fed == f) %>%
          prob_rank_random(K)
      },
      .progress = TRUE
    )
  )

per_fed_anal %>%
  mutate(pval = 2 * pmin(raw_pval, 1 - raw_pval)) %>%
  mutate(adj_pval = p.adjust(pval, method = "fdr"),
         .by = c(juniors, inactives, floor, K)) %>%
  summarize(
    # Number of federations (with at least 30-30 players):
    federations = n(),
    # Number of federations where women are significantly stronger:
    signif_F = sum(adj_pval < 0.05 & raw_pval >  0.5),
    # Number of federations where men are significantly stronger:
    signif_M = sum(adj_pval < 0.05 & raw_pval <= 0.5),
    # In which federation(s) are women stronger than men?
    feds_F = str_flatten_comma(fed[adj_pval < 0.05 & raw_pval > 0.5]),
    .by = c(juniors, inactives, floor, K)
  ) %>%
  mutate(feds_F = ifelse(feds_F == "", "-", feds_F)) %>%
  arrange(K) %>%
  print(n = Inf)

library(tidyverse)
library(Rcpp)

sourceCpp("permutation_table.cpp")

top1 <- max

participation_gap <- function(rating_data) {
  rating_data %>%
    count(fed, sex, name = "no_of_players") %>%
    pivot_wider(names_from = "sex", values_from = "no_of_players") %>%
    replace_na(list(`F` = 0, `M` = 0)) %>%
    mutate(participation_gap = `M` / (`F` + `M`))
}

federations <- function(rating_data, min_players) {
  participation_gap(rating_data) %>%
    mutate(no_minority = pmin(`F`, `M`)) %>%
    filter(no_minority >= min_players) %>%
    pull(fed)
}

restrict_data <- function(rating_data, include_junior = TRUE, include_inactive = FALSE,
                          min_rating = 1000, min_players = 30, birth_uncertain = FALSE) {
  if (include_junior) max_byear <- 2019 else max_byear <- 1999
  rating_data %>%
    filter(if (include_inactive) TRUE else active) %>%
    filter(if (birth_uncertain) TRUE else born != 0) %>%
    filter(born <= max_byear, rating >= min_rating) %>%
    filter(fed %in% federations(., min_players))
}

permut_tab <- function(x, y, fn = mean, perms) {
  if (identical(fn, mean)) {
    permtab_mean(x, y, perms)
  } else if (identical(fn, median)) {
    permtab_median(x, y, perms)
  } else if (identical(fn, sd)) {
    permtab_sd(x, y, perms)
  } else if (identical(fn, top1)) {
    permtab_top1(x, y, perms)
  } else if (identical(fn, top10)) {
    permtab_top10(x, y, perms)
  } else NA
}

perm_generator <- function(rating_data, juniors = TRUE, inactives = FALSE, floor = 1000,
                           fn = mean, perms) {
  rating_data %>%
    restrict_data(juniors, inactives, floor) %>%
    select(fed, sex, rating) %>%
    pivot_wider(names_from = sex, values_from = rating, values_fn = list) %>%
    mutate(permuts = map2(`F`, `M`, \(f, m) permut_tab(f, m, fn, perms)))
}


rating_data <- read_rds("../data/rating_data.rds")

crossing(juniors = c(FALSE, TRUE),
         inactives = c(FALSE, TRUE),
         floor = c(1000, 1400, 1600),
         fn = c(mean = mean, median = median, sd = sd, top1 = top1, top10 = top10)) %>%
  mutate(metric = names(fn)) %>%
  mutate(results = pmap(list(juniors, inactives, floor, fn),
                        perm_generator, rating_data = rating_data, perms = 100,
                        .progress = TRUE)) %>%
  unnest(results) %>%
  write_rds("../data/perm-data-1e5-perms.rds", compress = "xz")

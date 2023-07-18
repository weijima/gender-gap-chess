library(tidyverse)
library(Rcpp)

sourceCpp("code/permutation-table.cpp")

top1 <- max

restrict_data <- function(rating_data, include_junio, include_inactive, min_rating,
                          birth_uncertain = FALSE) {
  if (include_junior) max_byear <- 2019 else max_byear <- 1999
  rating_data %>%
    filter(if (include_inactive) TRUE else active) %>%
    filter(if (birth_uncertain) TRUE else born != 0) %>%
    filter(born <= max_byear | is.na(born), rating >= min_rating)
}

perm_generator <- function(rating_data, juniors, inactives, floor, fn, perms) {
  rating_data %>%
    restrict_data(juniors, inactives, floor) %>%
    select(sex, rating) %>%
    pivot_wider(names_from = sex, values_from = rating, values_fn = list) %>%
    mutate(permuts = map2(`M`, `F`, function(m, f) permut_tab(m, f, fn, perms)))
}


rating_data <- read_csv("data/rating-data.csv", col_types = "cccdiil")

crossing(juniors = c(FALSE, TRUE),
         inactives = c(FALSE, TRUE),
         floor = c(1000, 1400, 1600),
         fn = c(mean = mean, median = median, sd = sd, top1 = top1, top10 = top10)) %>%
  mutate(metric = names(fn)) %>%
  mutate(fn = unname(fn)) %>%
  mutate(results = pmap(list(juniors, inactives, floor, fn),
                        perm_generator, rating_data = rating_data, perms = 100000)) %>%
  unnest(results) %>%
  write_rds("data/perm-global.rds", compress = "xz")

library(tidyverse)
library(Rcpp)

sourceCpp("permutation_tests.cpp")

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

compare <- function(rating_data, colname, test = permut_test) {
  rating_data %>%
    select(fed, sex, rating) %>%
    pivot_wider(names_from = sex, values_from = rating, values_fn = list) %>%
    transmute(fed, "{{colname}}" := map2_dbl(`F`, `M`, test))
}

permut_test <- function(x, y, fun = mean, perms = 10000) {
  if (identical(fun, mean)) {
    permtest_mean(x, y, perms)
  } else if (identical(fun, median)) {
    permtest_median(x, y, perms)
  } else if (identical(fun, sd)) {
    permtest_sd(x, y, perms)
  } else if (identical(fun, top1)) {
    permtest_top1(x, y, perms)
  } else if (identical(fun, top10)) {
    permtest_top10(x, y, perms)
  } else NA
}

analyze_ratings <- function(rating_data, fn = mean, perms = 1000, test = permut_test) {
  rating_data %>%
    compare(pvalue, test = function(f, m) test(f, m, fn, perms)) %>%
    left_join(participation_gap(rating_data), by = "fed") %>%
    left_join(compare(rating_data, diff, function(f, m) fn(f) - fn(m)), by = "fed")
}

restrict_data <- function(rating_data, include_junior = TRUE, min_rating = 1000,
                          min_players = 30, include_inactive = FALSE,
                          birth_uncertain = FALSE) {
  if (include_junior) max_byear <- 2019 else max_byear <- 1999
  rating_data %>%
    filter(if (include_inactive) TRUE else active) %>%
    filter(if (birth_uncertain) TRUE else born != 0) %>%
    filter(born <= max_byear, rating >= min_rating) %>%
    filter(fed %in% federations(., min_players))
}

p_anal <- function(pvalues, signif = 0.05, method = "fdr") {
  p_female <- p.adjust(pvalues, method = method)
  p_male <- p.adjust(1 - pvalues, method = method)
  signif_female <- 2L * (p_female < signif)
  signif_male <- 1L * (p_male < signif)
  s <- signif_female + signif_male
  case_when(
    s == 2 ~ "female-slanted",
    s == 1 ~ "male-slanted",
    s == 0 ~ "nonsignificant",
    .default = "ERROR - BOTH MALES AND FEMALES ARE SIGNIFICANT"
  )
}

significance_counter <- function(rating_data, include_junior = TRUE,
                                 include_inactive = FALSE, min_rating = 1000,
                                 fn = mean, perms = 1000) {
  anal <- rating_data %>%
    restrict_data(include_junior = include_junior, min_rating = min_rating,
                  include_inactive = include_inactive) %>%
    analyze_ratings(fn, perms = perms, test = permut_test)
  sig_fdr <- anal %>%
    mutate(signif = p_anal(pvalue, method = "fdr")) %>%
    count(signif) %>%
    mutate(method = "fdr")
  sig_raw <- anal %>%
    mutate(signif = p_anal(pvalue, method = "none")) %>%
    count(signif) %>%
    mutate(method = "none")
  bind_rows(sig_fdr, sig_raw)
}



rating_data <- read_rds("../data/rating_data.rds")

tictoc::tic()
crossing(include_junior = c(FALSE, TRUE),
         include_inactive = c(FALSE, TRUE),
         min_rating = c(1000),
         fn = c(mean = mean, median = median, sd = sd, top1 = top1, top10 = top10)) %>%
  mutate(metric = names(fn)) %>%
  mutate(results = pmap(list(include_junior, include_inactive, min_rating, fn),
                        significance_counter, rating_data = rating_data,
                        perms = 100000)) %>%
  unnest(results) %>%
  select(-fn) %>%
  write_csv("../data/participation-reject.csv")
tictoc::toc()

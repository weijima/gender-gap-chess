library(tidyverse)



top10 <- function(x) mean(tail(sort(x), 10))


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


restrict_data <- function(rating_data, include_junior, include_inactive, min_rating,
                          min_players = 30, birth_uncertain = FALSE) {
  if (include_junior) max_byear <- 2019 else max_byear <- 1999
  rating_data %>%
    filter(if (include_inactive) TRUE else active) %>%
    filter(if (birth_uncertain) TRUE else !is.na(born)) %>%
    filter(born <= max_byear | is.na(born), rating >= min_rating) %>%
    filter(fed %in% federations(., min_players))
}


data_filter_labels <- function(data) {
  data %>%
    mutate(jun_inact = str_c(as.integer(juniors), as.integer(inactives))) %>%
    mutate(jun_inact = fct_relevel(jun_inact, "10", "11", "01", "00")) %>%
    arrange(jun_inact, floor) %>%
    select(!jun_inact) %>%
    mutate(juniors = ifelse(juniors, "Yes", "No"),
           inactives = ifelse(inactives, "Yes", "No"))
}



rating_data <- read_csv("data/rating-data.csv", col_types = "cccdiil")

null_data <- read_csv("data/null-stats.csv", show_col_types = FALSE)



# Permutation test results for global data across all metrics and data filters
null_data %>%
  filter(fed == "ALL") %>%
  select(!fed) %>%
  pivot_wider(names_from = stat, values_from = value) %>%
  mutate(pval = 2 * pmin(ptpval, 1 - ptpval)) %>%
  mutate(signif = case_when(
    pval <  0.05 & ptpval >= 0.5 ~ "women",
    pval <  0.05 & ptpval <  0.5 ~ "men",
    pval >= 0.05                 ~ ""
  )) %>%
  data_filter_labels() %>%
  mutate(across(obs | ptmean | ptsd, \(x) round(x, 1)), pval = round(pval, 4)) %>%
  select(!ptpval) %>%
  arrange(metric) %>%
  knitr::kable(format = "simple")



# Mann-Whitney and Kolmogorov-Smirnov tests
# Compile data:
global_data <-
  crossing(juniors = c(TRUE, FALSE),
           inactives = c(TRUE, FALSE),
           floor = c(1000, 1400, 1600)) %>%
  mutate(male = pmap(list(juniors, inactives, floor), \(juniors, inactives, floor) {
    rating_data %>%
      restrict_data(include_junior = juniors, include_inactive = inactives,
                    min_rating = floor, min_players = 0, birth_uncertain = FALSE) %>%
      filter(sex == "M") %>%
      pull(rating)
  } )) %>%
  mutate(female = pmap(list(juniors, inactives, floor), \(juniors, inactives, floor) {
    rating_data %>%
      restrict_data(include_junior = juniors, include_inactive = inactives,
                    min_rating = floor, min_players = 0, birth_uncertain = FALSE) %>%
      filter(sex == "F") %>%
      pull(rating)
  } ))

# Analysis:
global_data %>%
  mutate(MW = map2(male, female, wilcox.test, conf.int = TRUE)) %>%
  mutate(KS = map2(male, female, ks.test)) %>%
  mutate(MW = map(MW, broom::tidy), KS = map(KS, broom::tidy)) %>%
  unnest(c(MW, KS), names_sep = "_") %>%
  select(-contains("male"), -contains("method"), -contains("alternative")) %>%
  mutate(MW_statistic = as.character(MW_statistic),
         across(matches("MW_(e|c)"), round)) %>%
  write_csv("data/global-stat-data.csv")
# read_csv("data/global-stat-data.csv", col_types = "lliicdii")

# Number of significant Mann-Whitney test results per data filter:
crossing(juniors = c(TRUE, FALSE),
         inactives = c(TRUE, FALSE),
         floor = c(1000, 1400, 1600)) %>%
  mutate(p_MW = pmap(list(juniors, inactives, floor), \(juniors, inactives, floor) {
    rating_data %>%
      restrict_data(include_junior = juniors, include_inactive = inactives,
                    min_rating = floor, min_players = 30, birth_uncertain = FALSE) %>%
      nest(data = !fed) %>%
      mutate(p = map_dbl(data, \(x) wilcox.test(rating ~ sex, data = x)$p.value)) %>%
      pull(p)
  } )) %>%
  mutate(p_MW_adj = map(p_MW, p.adjust, method = "fdr")) %>%
  mutate(num_signif = map_dbl(p_MW_adj, \(x) length(x[x < 0.05])))

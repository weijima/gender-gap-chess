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

p_anal <- function(pvalues, signif = 0.05, method = "fdr") {
  p_female <- p.adjust(1 - pvalues, method = method)
  p_male <- p.adjust(pvalues, method = method)
  # The factor of 2 simply introduces a symbol to distinguish women (2) from men (1):
  signif_female <- 2L * (p_female < signif / 2)
  signif_male <- 1L * (p_male < signif / 2)
  # The nonzero entries of signif_female and signif_male are completely nonoverlapping:
  s <- signif_female + signif_male
  # Translate the arbitrary symbols 2 and 1 into test describing significance:
  case_when(
    s == 2 ~ "female-slanted",
    s == 1 ~ "male-slanted",
    s == 0 ~ "nonsignificant",
    .default = "ERROR - BOTH SEXES ARE SIGNIFICANT"
  )
}


rating_data <- read_csv("data/rating-data.csv", col_types = "cccdiil")
null_data <- read_csv("data/null-stats.csv", show_col_types = FALSE)


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
  mutate(signif = map_dbl(p_MW_adj, \(x) length(x[x < 0.05])))

# Distribution of p-values for just the baseline case:
crossing(juniors = c(TRUE, FALSE),
         inactives = c(TRUE, FALSE),
         floor = c(1000, 1400, 1600)) %>%
  slice(7) %>%
  mutate(p_MW = pmap(list(juniors, inactives, floor), \(juniors, inactives, floor) {
    rating_data %>%
      restrict_data(include_junior = juniors, include_inactive = inactives,
                    min_rating = floor, min_players = 30, birth_uncertain = FALSE) %>%
      nest(data = !fed) %>%
      mutate(p = map_dbl(data, \(x) wilcox.test(rating ~ sex, data = x)$p.value))
  } )) %>%
  unnest(p_MW) %>%
  mutate(p_adj = p.adjust(p, method = "fdr")) %>%
  #count(signif = p_adj < 0.05)
  pull(p_adj) %>%
  ks.test("punif")


# Mean:
global_data %>%
  mutate(mean_M = map_dbl(male, mean), mean_F = map_dbl(female, mean)) %>%
  mutate(mean_diff = map2_dbl(mean_M, mean_F, `-`))
null_data %>%
  filter(fed != "ALL") %>%
  filter(juniors, !inactives, floor == 1000) %>%
  filter(metric == "mean", stat == "ptpval") %>%
  #pull(value) %>% p.adjust(method = "fdr") %>% ks.test("punif")
  mutate(sig = p_anal(value, method = "none")) %>%
  count(sig)


# Median:
global_data %>%
  mutate(median_M = map_dbl(male, median), median_F = map_dbl(female, median)) %>%
  mutate(median_diff = map2_dbl(median_M, median_F, `-`))
null_data %>%
  filter(fed != "ALL") %>%
  filter(juniors, !inactives, floor == 1000) %>%
  filter(metric == "median", stat == "obs") %>%
  count(`higher female median` = value < 0)
null_data %>%
  filter(fed != "ALL") %>%
  filter(juniors, !inactives, floor == 1000) %>%
  filter(metric == "median", stat == "ptpval") %>%
  # pull(value) %>% p.adjust(method = "fdr") %>% ks.test("punif")
  mutate(sig = p_anal(value, method = "none")) %>%
  #filter(sig == "nonsignificant")
  count(sig)


# SD:
global_data %>%
  mutate(sd_M = map_dbl(male, sd), sd_F = map_dbl(female, sd))
null_data %>%
  filter(fed == "ALL") %>%
  filter(juniors, !inactives, floor == 1000) %>%
  filter(metric == "sd", stat == "ptpval")
null_data %>%
  filter(fed != "ALL") %>%
  filter(juniors, !inactives, floor == 1000) %>%
  filter(metric == "sd", stat == "obs") %>%
  count(`female higher` = value < 0)
null_data %>%
  filter(fed != "ALL") %>%
  filter(juniors, !inactives, floor == 1000) %>%
  filter(metric == "sd", stat == "ptpval") %>%
  mutate(sig = p_anal(value, method = "fdr")) %>%
  count(sig)
null_data %>%
  filter(fed != "ALL") %>%
  filter(juniors, !inactives, floor == 1000) %>%
  filter(metric == "sd", stat == "ptpval") %>%
  pull(value) %>%
  p.adjust(method = "fdr") %>%
  ks.test("punif")


# Top1:
global_data %>%
  mutate(top1_M = map_dbl(male, max), top1_F = map_dbl(female, max))
null_data %>%
  filter(fed == "ALL") %>%
  filter(juniors, !inactives, floor == 1000) %>%
  filter(metric == "top1")
null_data %>%
  filter(fed != "ALL") %>%
  filter(juniors, !inactives, floor == 1000) %>%
  filter(metric == "top1", stat == "obs") %>%
  count(`female higher` = value < 0)
null_data %>%
  filter(fed != "ALL") %>%
  filter(juniors, !inactives, floor == 1000) %>%
  filter(metric == "top1", stat == "obs") %>%
  filter(value < 0)
null_data %>%
  filter(fed != "ALL") %>%
  filter(juniors, !inactives, floor == 1000) %>%
  filter(metric == "top1", stat == "ptpval") %>%
  mutate(sig = p_anal(value, method = "fdr")) %>%
  count(sig)
null_data %>%
  filter(fed != "ALL") %>%
  filter(juniors, !inactives, floor == 1000) %>%
  filter(metric == "top1", stat == "ptpval") %>%
  pull(value) %>%
  p.adjust(method = "fdr") %>%
  ks.test("punif")


# Top10:
global_data %>%
  mutate(top10_M = map_dbl(male, top10), top10_F = map_dbl(female, top10))
null_data %>%
  filter(fed == "ALL") %>%
  filter(juniors, !inactives, floor == 1000) %>%
  filter(metric == "top10")
null_data %>%
  filter(fed != "ALL") %>%
  filter(juniors, !inactives, floor == 1000) %>%
  filter(metric == "top10", stat == "obs") %>%
  count(`female higher` = value < 0)
null_data %>%
  filter(fed != "ALL") %>%
  filter(juniors, !inactives, floor == 1000) %>%
  filter(metric == "top10", stat == "ptpval") %>%
  mutate(sig = p_anal(value, method = "fdr")) %>%
  count(sig)
null_data %>%
  filter(fed != "ALL") %>%
  filter(juniors, !inactives, floor == 1000) %>%
  filter(metric == "top10", stat == "ptpval") %>%
  pull(value) %>%
  p.adjust(method = "fdr") %>%
  ks.test("punif")

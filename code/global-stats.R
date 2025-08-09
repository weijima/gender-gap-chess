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

null_data <- read_csv("data/null-stats.csv", col_types = "llicccd")



# Permutation test results for global data across all metrics and data filters
perm_table <- null_data %>%
  filter(fed == "ALL") %>%
  select(!fed) %>%
  pivot_wider(names_from = stat, values_from = value) %>%
  mutate(pval = 2 * pmin(ptpval, 1 - ptpval)) %>%
  mutate(signif = case_when(
    pval <  0.05 & ptpval >= 0.5 ~ "(W)",
    pval <  0.05 & ptpval <  0.5 ~ "(M)",
    pval >= 0.05                 ~ ""
  )) %>%
  data_filter_labels() %>%
  select(!ptpval) %>%
  mutate(metric = fct_relevel(metric, "mean", "median", "top10", "top1", "sd")) %>%
  arrange(metric)

perm_table %>%
  mutate(metric = case_match(
    metric,
    "mean" ~ "Overall mean gap",
    "median" ~ "Overall median gap",
    "top10" ~ "Top 10 gap",
    "top1" ~ "Top 1 gap",
    "sd" ~ "Gap in standard deviation"
  )) %>%
  mutate(pt = str_c(sprintf("%.1f", ptmean), " $\\pm$ ", sprintf("%.1f", ptsd)),
         .keep = "unused") %>%
  mutate(obs = sprintf("%.1f", obs)) %>%
  mutate(pval = ifelse(pval == 0, "< 10\\textsuperscript{-4}",
                       sprintf("%.4f", pval))) %>%
  select(Metric = metric, Juniors = juniors, Inactives = inactives,
         `Rating floor` = floor, `Observed` = obs, `Permutation` = pt,
         `p-value` = pval, ` ` = signif) %>%
  kableExtra::kbl(
    format = "latex",
    booktabs = TRUE,
    longtable = FALSE,
    linesep = c(rep("", 11), "\\addlinespace"),
    escape = FALSE,
    align = "llllrrrl",
    label = "global-perm-SI",
    caption = str_c("Results for the global chess rating dataset. The first column ",
                    "is the statistic; the next three are various data filters; ",
                    "``Observed'' is the observed value of the metric (men minus ",
                    "women); `Permutation'' is the mean plus/minus one standard ",
                    "deviation of the one million permutation samples; and ",
                    "``p-value'' is the p-value (computed to four decimal precision ",
                    "via \\eqref{eq:ptrans}, where $f$ is the number of permutation ",
                    "samples that are less than the observed value) plus a ",
                    "parenthetical W or M in case the result is significant at the ",
                    "0.05 level favoring either women (W) or men (M).")
    )



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



# Global analysis of rating standard deviations
null_data %>%
  filter(metric == "sd" & fed == "ALL") %>%
  select(!metric & !fed) %>%
  filter(stat %in% c("obs", "ptpval")) %>%
  pivot_wider(names_from = stat, values_from = value) %>%
  mutate(stddevs = pmap(list(juniors, inactives, floor), \(jun, ina, fl) {
    restrict_data(rating_data, jun, ina, fl, min_players = 0) %>%
      summarize(sd = sd(rating), .by = sex) %>%
      pivot_wider(names_from = sex, values_from = sd, names_prefix = "sd_")
  } )) %>%
  unnest(stddevs) %>%
  data_filter_labels() %>%
  mutate(ptpval = 2 * pmin(ptpval, 1 - ptpval)) %>%
  select(Juniors = juniors, Inactives = inactives, `Rating floor` = floor,
         `sd(women)` = sd_F, `sd(men)` = sd_M, Difference = obs, `p-value` = ptpval) %>%
  kableExtra::kbl(format = "latex", booktabs = TRUE, longtable = FALSE,
                  linesep = "", escape = FALSE, digits = c(NA, NA, 0, 1, 1, 1, 4),
                  align = "lllrrrr",
                  label = "tab:global-perm-SI", caption = "")

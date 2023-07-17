library(tidyverse)

read_csv("data/null-stats.csv", show_col_types = FALSE) %>%
  filter(stat == "obs") %>%
  rename(y = value) %>%
  # Adjust values for different conventions (F - M to M - F):
  mutate(y = case_when(stat %in% c("obs", "ptmean") ~ -y, stat == "ptpval" ~ 1 - y,
                       .default = y)) %>%
  left_join(read_csv("data/age-experience-tab.csv", show_col_types = FALSE),
            by = join_by(metric, juniors, inactives, floor, fed)) %>%
  filter(metric %in% c("mean", "top1", "top10")) %>%
  select(-c(stat, E, A, weight)) %>%
  relocate(fed, .before = metric) %>%
  arrange(juniors, inactives, floor, fed, metric)

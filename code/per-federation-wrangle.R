library(tidyverse)

read_csv("../data/participation-reject.csv", show_col_types = FALSE) %>%
  mutate(Federations = sum(n), .by = c(contains("_"), method, metric)) %>%
  pivot_wider(names_from = signif, values_from = n, values_fill = 0) %>%
  mutate(Filter = str_c("J",1*include_junior,".I",1*include_inactive,".",min_rating)) %>%
  transmute(Filter, Federations, method, metric, s = `female-slanted`+`male-slanted`) %>%
  group_by(Filter, Federations, metric) %>%
  summarise(sig = str_c(s[method == "none"], " (", s[method == "fdr"], ")")) %>%
  ungroup() %>%
  pivot_wider(names_from = metric, values_from = sig)

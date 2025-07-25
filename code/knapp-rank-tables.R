library(tidyverse)


data_filter_labels <- function(data) {
  data %>%
    mutate(jun_inact = str_c(as.integer(juniors), as.integer(inactives))) %>%
    mutate(jun_inact = fct_relevel(jun_inact, "10", "11", "01", "00")) %>%
    arrange(jun_inact, floor) %>%
    mutate(juniors = ifelse(juniors, "With juniors, ", "W/o juniors, "),
           inactives = ifelse(inactives, "with inactives; ", "w/o inactives; "),
           floor = str_c("rating floor: ", floor)) %>%
    mutate(filter = str_c(juniors, inactives, floor), .before = 1) %>%
    select(!juniors & !inactives & !floor & !jun_inact)
}



# Global analysis

read_csv("data/knapp-rank-global.csv", col_types = "lliidd") %>%
  data_filter_labels() %>%
  arrange(K) %>%
  mutate(Significance = case_when(
    pval <  0.05 & raw_pval >= 0.5 ~ "slanted towards women",
    pval <  0.05 & raw_pval <  0.5 ~ "slanted towards men",
    pval >= 0.05                   ~ "nonsignificant"
  )) %>%
  select(!raw_pval) %>%
  rename(`Data filter` = filter, `p-value` = pval) %>%
  knitr::kable(format = "latex")



# Per-federation analysis

read_csv("data/knapp-rank-per-fed.csv", col_types = "lliciddd") %>%
  data_filter_labels() %>%
  summarize(
    # Number of federations (with at least 30-30 players):
    federations = n(),
    # Number of federations where women are significantly stronger:
    signif_F = sum(adj_pval < 0.05 & raw_pval >  0.5),
    # Number of federations where men are significantly stronger:
    signif_M = sum(adj_pval < 0.05 & raw_pval <= 0.5),
    # In which federation(s) are women stronger than men?
    feds_F = str_flatten_comma(fed[adj_pval < 0.05 & raw_pval > 0.5]),
    .by = c(filter, K)
  ) %>%
  arrange(K) %>%
  mutate(signif_F = ifelse(feds_F == "", signif_F, str_c(signif_F," (",feds_F,")"))) %>%
  rename(`Data filter` = filter, `Federations` = federations,
         `Federations with significantly stronger women` = signif_F,
         `Federations with significantly stronger men` = signif_M) %>%
  select(!feds_F) %>%
  knitr::kable(format = "latex")

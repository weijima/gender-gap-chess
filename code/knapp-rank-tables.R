library(tidyverse)


data_filter_labels <- function(data) {
  data %>%
    mutate(jun_inact = str_c(as.integer(juniors), as.integer(inactives))) %>%
    mutate(jun_inact = fct_relevel(jun_inact, "10", "11", "01", "00")) %>%
    arrange(jun_inact, floor) %>%
    select(!jun_inact) %>%
    mutate(juniors = ifelse(juniors, "Yes", "No"),
           inactives = ifelse(inactives, "Yes", "No"))
}



# Global analysis
read_csv("data/knapp-rank-global.csv", col_types = "lliidd") %>%
  data_filter_labels() %>%
  arrange(K) %>%
  mutate(Significance = case_when(
    pval <  0.05 & raw_pval >= 0.5 ~ "women",
    pval <  0.05 & raw_pval <  0.5 ~ "men",
    pval >= 0.05                   ~ ""
  )) %>%
  select(!raw_pval) %>%
  mutate(pval = round(pval, 4)) %>%
  rename_with(str_to_title) %>%
  rename(`p-value` = Pval, `Rating floor` = Floor) %>%
  knitr::kable(format = "simple")



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
    .by = c(juniors, inactives, floor, K)
  ) %>%
  arrange(K) %>%
  mutate(signif_F = ifelse(feds_F == "", signif_F, str_c(signif_F," (",feds_F,")"))) %>%
  rename_with(str_to_title) %>%
  rename(`Rating floor` = Floor,
         `Women stronger` = Signif_f,
         `Men stronger` = Signif_m) %>%
  select(!Feds_f) %>%
  knitr::kable(format = "simple")

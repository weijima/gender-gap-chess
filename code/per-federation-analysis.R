library(tidyverse)


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
    .default = "ERROR - BOTH MALES AND FEMALES ARE SIGNIFICANT"
  )
}


# Table of raw p-values, along with the corrected significance for each federation:
pvalues <- read_csv("data/null-stats.csv", show_col_types = FALSE) %>%
  filter(fed != "ALL", stat == "ptpval") %>%
  select(-stat) %>%
  mutate(fdr = p_anal(value),
         none = p_anal(value, method = "none"),
         .by = c(juniors, inactives, floor, metric)) %>%
  pivot_longer(cols = c(fdr, none), names_to = "method", values_to = "signif")

# Generate Table 2 from the manuscript:
pvalues %>%
  summarise(n = n(), .by = c(juniors, inactives, floor, metric, method, signif)) %>%
  mutate(feds = sum(n), .by = c(juniors, inactives, floor, method, metric)) %>%
  pivot_wider(names_from = signif, values_from = n, values_fill = 0) %>%
  mutate(juniors = ifelse(juniors, "Yes", "No"),
         inactives = ifelse(inactives, "Yes", "No"),
         s = `female-slanted` + `male-slanted`) %>%
  select(-contains("-")) %>%
  summarise(sig = str_c(s[method == "none"]," (", s[method == "fdr"], ")"),
            .by = c(juniors, inactives, floor, feds, metric)) %>%
  mutate(metric = str_to_title(metric),
         metric = ifelse(metric == "Sd", "SD", metric)) %>%
  mutate(floor = ifelse(floor == 1000, 0, floor)) %>%
  pivot_wider(names_from = metric, values_from = sig) %>%
  rename(`Junior players` = juniors, `Inactive players` = inactives,
         `No. of federations` = feds, `Rating floor` = floor) %>%
  knitr::kable(format = "latex")

# Sample part of the data:
pvalues %>%
  filter(!juniors, !inactives, floor == 1400, method == "fdr") %>%
  summarise(n = n(), .by = c(juniors, inactives, floor, metric, method, signif)) %>%
  mutate(n = n / sum(n), .by = c(juniors, inactives, floor, metric, method)) %>%
  mutate(n = str_c(round(100 * n, 2), "%")) %>%
  pivot_wider(names_from = signif, values_from = n, values_fill = "0%")

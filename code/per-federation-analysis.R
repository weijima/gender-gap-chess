library(tidyverse)


signif_anal <- function(pvalues, raw_pvalues, signif = 0.05) {
  case_when(
    pvalues <  signif & raw_pvalues >= 0.5 ~ "female-slanted",
    pvalues <  signif & raw_pvalues <  0.5 ~ "male-slanted",
    pvalues >= signif                      ~ "nonsignificant"
  )
}


# Table of p-values, along with the corrected significance for each federation:
pvalues <- read_csv("data/null-stats.csv", col_types = "llicccd") %>%
  filter(fed != "ALL", stat == "ptpval") %>%
  select(!stat) %>%
  # Convert two-sided p-values to one-sided:
  mutate(pval = 2 * pmin(value, 1 - value)) %>%
  # Adjust p-values for multiple comparisons:
  mutate(adj_pval = p.adjust(pval, method = "fdr"),
         .by = c(juniors, inactives, floor, metric)) %>%
  # Assess significance:
  mutate(fdr = signif_anal(adj_pval, value),
         none = signif_anal(pval, value)) %>%
  pivot_longer(cols = c(fdr, none), names_to = "method", values_to = "signif")

# Generate federation-significance table:
pvalues %>%
  summarise(n = n(), .by = c(juniors, inactives, floor, metric, method, signif)) %>%
  mutate(feds = sum(n), .by = c(juniors, inactives, floor, method, metric)) %>%
  pivot_wider(names_from = signif, values_from = n, values_fill = 0) %>%
  mutate(juniors = ifelse(juniors, "Yes", "No"),
         inactives = ifelse(inactives, "Yes", "No"),
         s = `female-slanted` + `male-slanted`) %>%
  select(!contains("-")) %>%
  summarise(sig = str_c(s[method == "none"]," (", s[method == "fdr"], ")"),
            .by = c(juniors, inactives, floor, feds, metric)) %>%
  mutate(metric = str_to_title(metric),
         metric = ifelse(metric == "Sd", "SD", metric)) %>%
  mutate(floor = ifelse(floor == 1000, 0, floor)) %>%
  pivot_wider(names_from = metric, values_from = sig) %>%
  rename(`Junior players` = juniors, `Inactive players` = inactives,
         `No. of federations` = feds, `Rating floor` = floor) %>%
  knitr::kable(format = "latex")

# The data in terms of percentage of significant federations:
pvalues %>%
  summarise(n = n(), .by = c(juniors, inactives, floor, metric, method, signif)) %>%
  mutate(n = n / sum(n), .by = c(juniors, inactives, floor, metric, method)) %>%
  mutate(n = str_c(round(100 * n, 2), "%")) %>%
  pivot_wider(names_from = signif, values_from = n, values_fill = "0%") %>%
  filter(method == "fdr")

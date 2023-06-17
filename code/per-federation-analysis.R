library(tidyverse)

pAnal <- function(pvalues, signif = 0.05, method = "fdr") {
  p_female <- p.adjust(pvalues, method = method)
  p_male <- p.adjust(1 - pvalues, method = method)
  signif_female <- 2L * (p_female < signif / 2)
  signif_male <- 1L * (p_male < signif / 2)
  s <- signif_female + signif_male
  case_when(
    s == 2 ~ "female-slanted",
    s == 1 ~ "male-slanted",
    s == 0 ~ "nonsignificant",
    .default = "ERROR - BOTH MALES AND FEMALES ARE SIGNIFICANT"
  )
}


# Table of raw p-values, along with the corrected significance for each federation:
pvalues <- read_rds("data/nulls/nulls-gyuri.rds") %>%
  filter(stat == "ptpval") %>%
  select(-stat) %>%
  mutate(fdr = pAnal(value),
         none = pAnal(value, method = "none"),
         .by = c(juniors, inactives, floor, metric)) %>%
  pivot_longer(cols = c(fdr, none), names_to = "method", values_to = "signif")

# Generate Table 2 from the manuscript:
pvalues %>%
  summarise(n = n(), .by = c(juniors, inactives, floor, metric, method, signif)) %>%
  mutate(Federations = sum(n), .by = c(juniors, inactives, floor, method, metric)) %>%
  pivot_wider(names_from = signif, values_from = n, values_fill = 0) %>%
  mutate(Filter = str_c("J", 1 * juniors, ".I", 1 * inactives, ".", floor)) %>%
  transmute(Filter, Federations, method, metric, s = `female-slanted`+`male-slanted`) %>%
  group_by(Filter, Federations, metric) %>%
  summarise(sig = str_c(s[method=="none"]," (",s[method=="fdr"],")"), .groups="drop") %>%
  pivot_wider(names_from = metric, values_from = sig)

# Sample part of the data:
pvalues %>%
  filter(!juniors, !inactives, floor == 1400, method == "fdr") %>%
  summarise(n = n(), .by = c(juniors, inactives, floor, metric, method, signif)) %>%
  mutate(n = n / sum(n), .by = c(juniors, inactives, floor, metric, method)) %>%
  mutate(n = str_c(round(100 * n, 2), "%")) %>%
  pivot_wider(names_from = signif, values_from = n, values_fill = "0%")

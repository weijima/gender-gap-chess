library(tidyverse)

top1 <- max

top10 <- \(x) sort(x) |> tail(10) |> mean()

obsDiff <- \(women, men, metric) match.fun(metric)(women) - match.fun(metric)(men)

rawPval <- \(women, men, metric, permuts) {
  obs <- obsDiff(women, men, metric)
  length(permuts[obs < permuts]) / length(permuts)
}

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


dat <- read_rds("../data/large_data/perm-data-1e5-perms.rds")

# dat |>
#   filter(!juniors, !inactives, floor == 1000, fed == "IND", metric == "mean") |>
#   transmute(permuts, obs = pmap_dbl(list(`F`, `M`, metric), obsDiff)) |>
#   unnest(permuts) |>
#   ggplot(aes(x = permuts)) +
#   geom_histogram(bins = 30, colour = "steelblue", fill = "steelblue", alpha = 0.2) +
#   geom_vline(aes(xintercept = first(obs)), colour = "firebrick") +
#   theme_bw()

pvalues <- dat |>
  mutate(p = pmap_dbl(list(`F`, `M`, metric, permuts), rawPval)) |>
  select(-c(`fn`, `F`, `M`, `permuts`))

pvalues |>
  mutate(fdr = pAnal(p), none = pAnal(p, method = "none"),
         .by = c(juniors, inactives, floor, metric)) |>
  pivot_longer(cols = c(fdr, none), names_to = "method", values_to = "signif") |>
  summarise(n = n(), .by = c(juniors, inactives, floor, metric, method, signif)) |>
  mutate(Federations = sum(n), .by = c(juniors, inactives, floor, method, metric)) |>
  pivot_wider(names_from = signif, values_from = n, values_fill = 0) |>
  mutate(Filter = str_c("J", 1 * juniors, ".I", 1 * inactives, ".", floor)) |>
  transmute(Filter, Federations, method, metric, s = `female-slanted`+`male-slanted`) |>
  group_by(Filter, Federations, metric) |>
  summarise(sig = str_c(s[method=="none"]," (",s[method=="fdr"],")"), .groups="drop") |>
  pivot_wider(names_from = metric, values_from = sig)

pvalues |>
  mutate(fdr = pAnal(p), none = pAnal(p, method = "none"),
         .by = c(juniors, inactives, floor, metric)) |>
  pivot_longer(cols = c(fdr, none), names_to = "method", values_to = "signif") |>
  summarise(n = n(), .by = c(juniors, inactives, floor, metric, method, signif)) |>
  filter(!juniors, !inactives, floor == 1400, method == "fdr")

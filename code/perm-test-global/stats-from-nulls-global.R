library(tidyverse)

top1 <- max

top10 <- function(x) mean(tail(sort(x), 10))

obs_diff <- function(m, w, metric) match.fun(metric)(m) - match.fun(metric)(w)

raw_pval <- function(m, w, metric, permuts) {
  obs <- obs_diff(m, w, metric)
  length(permuts[obs > permuts]) / length(permuts)
}


permtab <- tibble(juniors = logical(), inactives = logical(), floor = numeric(),
                  metric = character(), stat = character(), value = numeric())
for (file in Sys.glob("data/permdat/*global*.rds")) {
  cat(paste0(file, "..."))
  permtab <- read_rds(file) %>%
    mutate(obs = pmap_dbl(list(`F`, `M`, metric), obs_diff),
           ptpval = pmap_dbl(list(`F`, `M`, metric, permuts), raw_pval),
           ptmean = map_dbl(permuts, mean),
           ptsd = map_dbl(permuts, sd)) %>%
    select(-c(fn, `F`, `M`, permuts)) %>%
    pivot_longer(cols = c(obs, ptpval, ptmean, ptsd), names_to = "stat") %>%
    bind_rows(permtab, .)
  cat(paste0("done\n"))
}

permtab %>%
  mutate(fed = "ALL", .after = metric) %>%
  arrange(juniors, inactives, floor, metric) %>%
  write_csv("data/null-stats-global.csv")

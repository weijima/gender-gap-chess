library(tidyverse)

top1 <- max

top10 <- function(x) mean(tail(sort(x), 10))

obsDiff <- function(w, m, metric) match.fun(metric)(w) - match.fun(metric)(m)

read_csv("data/permdat/master.csv", show_col_types = FALSE) %>%
  filter(!juniors, !inactives, floor == 1400, metric == "top1") %>%
  transmute(data = map(file, read_rds)) %>%
  unnest(data) %>%
  filter(fed == "ARG") %>%
  mutate(obs = pmap_dbl(list(`F`, `M`, metric), obsDiff)) %>%
  select(obs, permuts) %>%
  unnest(permuts) %>%
  ggplot(aes(x = permuts)) +
  geom_histogram(colour = "steelblue", fill = "steelblue", alpha = 0.2, bins = 40) +
  geom_vline(aes(xintercept = first(obs)), colour = "firebrick") +
  theme_bw()

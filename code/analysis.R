library(tidyverse)

dat <- read_rds("../data/large_data/perm-data-1e5-perms.rds")

dat |>
  filter(!juniors, !inactives, floor == 1000, fed == "TUR", metric == "mean") |>
  transmute(permuts, obs = map2_dbl(`F`, `M`, \(w, m) mean(w) - mean(m))) |>
  unnest(permuts) |>
  ggplot(aes(x = permuts)) +
  geom_histogram(bins = 30, colour = "steelblue", fill = "steelblue", alpha = 0.2) +
  geom_vline(aes(xintercept = first(obs)), colour = "firebrick") +
  theme_bw()

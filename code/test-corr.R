library(tidyverse)
library(ggfortify)

read_csv("data/rating-data.csv", col_types = "cccdiil") %>%
  filter(rating < 1600) %>%
  ggplot(aes(x = games, y = rating)) +
  geom_point(alpha = 0.1, colour = "steelblue") +
  geom_smooth(method = lm, se = FALSE) +
  scale_y_continuous(limits = c(1000, 1600)) +
  facet_wrap(~ sex) +
  theme_classic()

read_csv("data/rating-data.csv", col_types = "cccdiil") %>%
  filter(rating < 1600) %>%
  ggplot(aes(x = games, y = rating)) +
  geom_density_2d_filled() +
  scale_x_log10() +
  facet_wrap(~ sex) +
  theme_classic()

read_csv("data/rating-data.csv", col_types = "cccdiil") %>%
  filter(rating < 1600, games > 0) %>%
  mutate(log10games = log10(games)) %>%
  nest(data = !sex) %>%
  mutate(corr = map_dbl(data, \(x) cor(x$log10games, x$rating))) %>%
  mutate(lm = map(data, \(x) lm(rating ~ log10games, data = x))) %>%
  mutate(lmSum = map(lm, summary)) %>%
  mutate(lmTidy = map(lmSum, broom::tidy)) %>%
  mutate(dia = map(lm, \(x) autoplot(x, smooth.colour = NA, alpha = 0.1))) %>%
  unnest(lmTidy) %>%
  select(sex, term, estimate, std.error, p.value) %>%
  mutate(across(c(estimate, std.error, p.value), \(x) round(x, 2))) %>%
  mutate(term = ifelse(term == "(Intercept)", "intercept", "slope"))

library(tidyverse)

read_csv("data/rating-data.csv", col_types = "cccdiil") %>%
  mutate(ratingBin = cut(rating, breaks = seq(1000, 2600, by = 50),
                         labels = seq(1000, 2550, by = 50))) %>%
  summarise(xp = mean(games), .by = c(sex, ratingBin)) %>%
  pivot_wider(names_from = sex, values_from = xp) %>%
  mutate(diff = `M` - `F`) %>%
  ggplot() +
  aes(x = ratingBin, y = diff) +
  geom_col(colour = "steelblue", fill = "steelblue", alpha = 0.2) +
  scale_x_discrete(name = "rating bin", breaks = c(1000, 1500, 2000, 2500)) +
  labs(y = "diff. mean experience (men - women)") +
  theme_bw()

library(tidyverse)

rating_data <- read_rds("data/rating_data.rds")

rating_data %>%
  mutate(ratingBin = cut(rating, breaks = seq(1000, 2600, by = 50),
                         labels = seq(1000, 2550, by = 50))) %>%
  group_by(sex, ratingBin) %>%
  summarise(xp = mean(games)) %>%
  ungroup() %>%
  pivot_wider(names_from = sex, values_from = xp) %>%
  mutate(diff = `F` - `M`) %>%
  ggplot() +
  aes(x = ratingBin, y = diff) +
  geom_col(colour = "steelblue", fill = "steelblue", alpha = 0.2) +
  scale_x_discrete(name = "rating bin", breaks = c(1000, 1500, 2000, 2500)) +
  labs(y = "difference in mean experience") +
  theme_bw()

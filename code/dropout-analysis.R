library(tidyverse)


inactive_ids <-
  read_csv("data/rating-data.csv", col_types = "cccdiil") %>%
  filter(!active) %>%
  select(id)


data_inact <-
  tibble(file = Sys.glob("data/tmp/standard_*.rds")) %>%
  mutate(data = map(file, read_rds, .progress = TRUE)) %>%
  unnest(data) %>%
  select(!file) %>%
  right_join(inactive_ids, by = join_by(id)) %>%
  arrange(year, month) %>%
  filter(born != "0000" & born != "0") %>%
  filter(!is.na(flag)) %>%
  slice(1L, .by = id)


data_inact %>%
  mutate(age = 2019L - as.integer(born)) %>%
  filter(age <= 80) %>%
  filter(!(year == 2012 & month == 10)) %>%
  mutate(sex = fct_relevel(ifelse(sex == "F", "Women", "Men"), "Women", "Men")) %>%
  ggplot(aes(x = age, colour = sex, fill = sex)) +
  geom_density(alpha = 0.3) +
  scale_colour_viridis_d(option = "C", end = 0.8) +
  scale_fill_viridis_d(option = "C", end = 0.8) +
  labs(x = "Age at becoming inactive", y = "Density") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_line(colour = "grey80"),
        panel.border = element_rect(colour = "grey80", fill = NA),
        panel.background = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom",
        strip.background = element_blank(),
        strip.placement = "outside")
#ggsave("figures/dropout.pdf", width = 4, height = 3.4)

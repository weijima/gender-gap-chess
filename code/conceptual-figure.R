library(tidyverse)


tibble(Rating = seq(1000, 2900, by = 100)) %>%
  mutate(Men_1 = dnorm(Rating, 1600, 350), Men_2 = Men_1, Men_3 = Men_1) %>%
  mutate(Women_1 = Men_1 * (tanh((2200 - Rating) / 50) + 1) / 2) %>%
  mutate(Women_2 = dnorm(Rating, 1400, 350)) %>%
  mutate(Women_3 = Men_1 * (1 + 0.2*exp(-(Rating - 1400) / 180))) %>%
  pivot_longer(!Rating, values_to = "Proportion") %>%
  separate(col = name, into = c("gender", "scenario"), sep = "_") %>%
  mutate(Proportion = Proportion / sum(Proportion), .by = c(gender, scenario)) %>%
  mutate(gender = fct_relevel(gender, "Women", "Men")) %>%
  mutate(scenario = as_factor(case_when(
    scenario == "1" ~ str_c("High-rated women\nunderrepresented"),
    scenario == "2" ~ str_c("Shifted rating\ndistributions"),
    scenario == "3" ~ str_c("Low-rated women\noverrepresented")
  ))) %>%
  ggplot() +
  geom_line(aes(x = Rating, y = Proportion, colour = gender)) +
  geom_area(aes(x = Rating, y = Proportion, fill = gender),
            alpha = 0.15, position = "identity") +
  scale_x_continuous(expand = c(0, 0), breaks = c(1000, 1700, 2400)) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 0.16),
                     breaks = c(0, 0.05, 0.1, 0.15), expand = c(0, 0)) +
  scale_colour_viridis_d(option = "C", end = 0.8) +
  scale_fill_viridis_d(option = "C", end = 0.8) +
  guides(fill = "none") +
  facet_wrap(~ scenario) +
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
#ggsave("figures/conceptual.pdf", width = 4.8, height = 2.5)

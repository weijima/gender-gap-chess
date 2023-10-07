library(tidyverse)

restrict_data <- function(juniors, inactives, floor,
                          rating_data, birth_uncertain = FALSE) {
  if (juniors) max_byear <- 2019 else max_byear <- 1999
  rating_data %>%
    filter(if (inactives) TRUE else active) %>%
    filter(if (birth_uncertain) TRUE else !is.na(born)) %>%
    filter(born <= max_byear | is.na(born), rating >= floor)
}

rating_data <- read_csv("data/rating-data.csv", col_types = "cccdiil") %>% as_tibble()

global_data <- crossing(juniors = c(TRUE, FALSE),
                        inactives = c(TRUE, FALSE),
                        floor = c(1000, 1400, 1600)) %>%
  mutate(dat = pmap(., restrict_data, rating_data = rating_data)) %>%
  unnest(dat) %>%
  select(juniors, inactives, floor, sex, rating)

global_data %>%
  mutate(juniors = ifelse(juniors, "With juniors, ", "No juniors, ")) %>%
  mutate(inactives = ifelse(inactives, "with inactives", "no inactives")) %>%
  mutate(filter = str_c(juniors, inactives), .before = 1) %>%
  mutate(filter = fct_relevel(filter, "With juniors, with inactives",
                              "With juniors, no inactives",
                              "No juniors, with inactives")) %>%
  select(-juniors, -inactives) %>%
  mutate(sex = ifelse(sex == "F", "Women", "Men")) %>%
  mutate(floor = str_c("Rating floor: ", floor)) %>%
  ggplot(aes(x = rating, y = after_stat(density), colour = sex, fill = sex)) +
  geom_area(alpha = 0.15, stat = "bin", binwidth = 100, position = "identity") +
  labs(x = "Rating", y = "Proportion") +
  facet_grid(filter ~ floor, scales = "free", switch = "y") +
  scale_colour_manual(values = c("goldenrod", "steelblue")) +
  scale_fill_manual(values = c("goldenrod", "steelblue")) +
  scale_x_continuous(limits = c(NA, 2900)) +
  theme_minimal() +
  theme(axis.line = element_line(colour = "grey80"),
        axis.ticks = element_line(colour = "grey80"),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.93, 0.44),
        strip.placement = "outside")
#ggsave("figures/global-fig.pdf", width = 8, height = 7)

read_csv("data/global-stat-data.csv", col_types = "llidcddddd") %>%
  select(-MW_estimate, -contains("statistic")) %>%
  filter(KS_p.value >= 2.3e-9)

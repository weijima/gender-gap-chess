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
  mutate(label = c("J","K","L","G","H","I","D","E","F","A","B","C")) %>%
  unnest(dat) %>%
  select(juniors, inactives, floor, sex, rating, label)

global_data %>%
  mutate(juniors = ifelse(juniors, "With juniors, ", "No juniors, ")) %>%
  mutate(inactives = ifelse(inactives, "with inactives", "no inactives")) %>%
  mutate(filter = str_c(juniors, inactives), .before = 1) %>%
  mutate(filter = fct_relevel(filter, "With juniors, with inactives",
                              "With juniors, no inactives",
                              "No juniors, with inactives")) %>%
  select(-juniors, -inactives) %>%
  mutate(sex = ifelse(sex == "F", "Women", "Men")) %>%
  mutate(floor_txt = str_c("Rating floor: ", floor)) %>%
  ggplot() +
  geom_area(aes(x = rating, y = after_stat(density), colour = sex, fill = sex),
            alpha = 0.15, stat = "bin", binwidth = 100, position = "identity") +
  geom_text(data = . %>% select(filter, floor_txt, floor, label) %>% distinct(),
            aes(label = label, x = floor), y = 0.002) +
  labs(x = "Rating", y = "Proportion") +
  facet_grid(filter ~ floor_txt, scales = "free_x", switch = "y") +
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
#ggsave("figures/global-fig.pdf", width = 7, height = 7)

read_csv("data/global-stat-data.csv", col_types = "llidcddddd") %>%
  select(-contains("statistic"), -contains("conf")) %>%
  filter(KS_p.value >= 2.3e-9)

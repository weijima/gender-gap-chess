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
  mutate(label = c("J","K","L","G","H","I","D","E","F","A","B","C")) %>%
  mutate(filter = case_when(
    juniors & inactives   ~ "With juniors,\nwith inactives",
    juniors & !inactives  ~ "With juniors,\nno inactives",
    !juniors & inactives  ~ "No juniors,\nwith inactives",
    !juniors & !inactives ~ "No juniors,\nno inactives"
  )) %>%
  mutate(filter = fct_relevel(filter, "With juniors,\nwith inactives",
                              "With juniors,\nno inactives",
                              "No juniors,\nwith inactives")) %>%
  mutate(floor_txt = str_c("Rating floor: ", floor)) %>%
  mutate(dat = pmap(list(juniors, inactives, floor),
                    restrict_data, rating_data = rating_data)) %>%
  select(-juniors, -inactives) %>%
  unnest(dat)

global_data %>%
  mutate(rating = cut(rating, breaks = 100*10:29, labels = 100*10:28, right = FALSE)) %>%
  mutate(sex = ifelse(sex == "F", "Women", "Men")) %>%
  summarise(N = n(), .by = c(floor, label, filter, floor_txt, sex, rating)) %>%
  mutate(prop = N / sum(N), .by = c(floor, label, filter, floor_txt, sex)) %>%
  mutate(rating = 100 * (9 + as.numeric(rating))) %>%
  ggplot() +
  geom_area(aes(x = rating, y = prop, colour = sex, fill = sex),
            position = "identity", alpha = 0.15) +
  #geom_text(data = . %>% select(filter, floor_txt, floor, label) %>% distinct(),
  #          aes(label = label, x = floor + 50), y = 0.2) +
  facet_grid(filter ~ floor_txt, scales = "free_x", switch = "y") +
  scale_x_continuous(name = "Rating", expand = c(0, 0), limits = c(NA, 2900),
                     breaks = 1000 + 500 * 0:3) +
  scale_y_continuous(name = "Proportion", labels = scales::percent,
                     breaks = 0:2 / 10, expand = c(0, 0)) +
  scale_colour_manual(values = c("goldenrod", "steelblue")) +
  scale_fill_manual(values = c("goldenrod", "steelblue")) +
  theme_minimal() +
  theme(axis.line = element_line(colour = "grey80"),
        axis.ticks = element_line(colour = "grey80"),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.92, 0.45),
        strip.placement = "outside")
#ggsave("figures/global-fig.pdf", width = 4.8, height = 4.8)

rating_data %>%
  restrict_data(juniors = TRUE, inactives = TRUE, floor = 1000, rating_data = .) %>%
  filter(games != 0) %>%
  mutate(log10games = log10(games)) %>%
  filter(rating < 1600) %>%
  #boxplot(log10games ~ sex, data = .)
  t.test(log10games ~ sex, data = ., conf.int = TRUE)

rating_data %>%
  restrict_data(juniors = TRUE, inactives = TRUE, floor = 1000, rating_data = .) %>%
  filter(rating < 1400) %>%
  count(sex, active) %>%
  mutate(active = ifelse(active, "active", "inactive")) %>%
  mutate(sex, active, frac = n / sum(n), .by = sex) %>%
  select(-n) %>%
  pivot_wider(names_from = sex, values_from = frac)

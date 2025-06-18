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
  left_join(read_csv("data/global-stat-data.csv", col_types = "lliicdii") %>%
              select(juniors, inactives, floor, KS_statistic),
            by = join_by(juniors, inactives, floor)) %>%
  rename(D = KS_statistic) %>%
  mutate(filter = case_when(
    juniors & inactives   ~ "With juniors,\nwith inactives",
    juniors & !inactives  ~ "With juniors,\nw/o inactives",
    !juniors & inactives  ~ "W/o juniors,\nwith inactives",
    !juniors & !inactives ~ "W/o juniors,\nw/o inactives"
  )) %>%
  mutate(filter = fct_relevel(filter, "With juniors,\nw/o inactives",
                              "With juniors,\nwith inactives",
                              "W/o juniors,\nwith inactives")) %>%
  mutate(floor_txt = str_c("Rating floor: ", floor)) %>%
  mutate(dat = pmap(list(juniors, inactives, floor),
                    restrict_data, rating_data = rating_data)) %>%
  select(!juniors & !inactives) %>%
  unnest(dat)


plot_data <- global_data %>%
  mutate(rating = cut(rating, breaks = 100*10:29, labels = 100*10:28, right = FALSE)) %>%
  mutate(sex = fct_relevel(ifelse(sex == "F", "Women", "Men"), "Women", "Men")) %>%
  summarise(N = n(), .by = c(floor, filter, floor_txt, sex, rating, D)) %>%
  mutate(prop = N / sum(N), .by = c(floor, filter, floor_txt, sex)) %>%
  mutate(rating = 100 * (9 + as.numeric(rating)))

label_data <- plot_data %>%
  distinct(floor_txt, filter, D) %>%
  arrange(filter, floor_txt) %>%
  mutate(label = LETTERS[1:12], x = rep(c(1200, 1600, 1800), 4))


plot_data %>%
  ggplot() +
  geom_line(aes(x = rating, y = prop, colour = sex)) +
  geom_area(aes(x = rating, y = prop, fill = sex),
            colour = NA, position = "identity", alpha = 0.15) +
  geom_text(data = label_data, x = 2500, y = 0.21, size = 3, parse = TRUE,
            aes(label = str_c("italic(D) == ", round(D, 2)))) +
  geom_vline(data = . %>% select(filter, floor_txt, floor) %>% distinct(),
             aes(xintercept = floor), linetype = "dashed", alpha = 0.5) +
  labs(x = "Rating", y = "Proportion") +
  facet_grid(filter ~ floor_txt, scales = "free_x", switch = "y") +
  scale_x_continuous(limits = c(1000,2900), breaks = c(1000,1700,2400), expand = c(0,0))+
  scale_y_continuous(labels = scales::percent, breaks = 0:2 / 10, expand = c(0, 0)) +
  scale_colour_viridis_d(option = "C", end = 0.8) +
  scale_fill_viridis_d(option = "C", end = 0.8) +
  guides(fill = "none") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.line = element_line(colour = "grey80"),
        axis.ticks = element_line(colour = "grey80"),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom",
        strip.background = element_blank(),
        strip.placement = "outside")
#ggsave("figures/global-fig.pdf", width = 4.8, height = 5.2)

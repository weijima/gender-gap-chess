library(tidyverse)


restrict_data <- function(rating_data, juniors, inactives, floor,
                          birth_uncertain = FALSE) {
  if (juniors) max_byear <- 2019 else max_byear <- 1999
  rating_data %>%
    filter(if (inactives) TRUE else active) %>%
    filter(if (birth_uncertain) TRUE else !is.na(born)) %>%
    filter(born <= max_byear | is.na(born), rating >= floor)
}


gender_tab <- function(rating_data, property, top_n) {
  col <- sym(property)
  rating_data %>%
    arrange(fed, sex, rating) %>%
    slice_tail(n = top_n, by = c(fed, sex)) %>%
    select(fed, sex, !!col) %>%
    summarise(prop = mean(!!col), .by = c(fed, sex)) %>%
    pivot_wider(names_from = sex, values_from = prop)
}



rating_data <- read_csv("data/rating-data.csv", col_types = "ccciiil") %>%
  filter(!is.na(born)) %>%
  mutate(age = 2019L - born)


main_tab <-
  crossing(juniors = c(FALSE, TRUE),
           inactives = c(FALSE, TRUE),
           floor = c(1000, 1400, 1600),
           property = c("games", "age"),
           metric = c("mean", "top10", "top1")) %>%
  mutate(data = pmap(list(juniors, inactives, floor, property, metric),
                     \(juniors, inactives, floor, property, metric) {
                       top_n <- case_match(metric, "mean"~Inf, "top10"~10, "top1"~1)
                       rating_data %>%
                         restrict_data(juniors, inactives, floor) %>%
                         gender_tab(property, top_n)
                     })
  ) %>%
  unnest(data)


main_tab %>%
  filter(property == "games") %>%
  mutate(metric = case_when(
    metric == "mean" ~ "Mean gap",
    metric == "top10" ~ "Top 10 gap",
    metric == "top1" ~ "Top gap "
  )) %>%
  mutate(metric = fct_relevel(metric, "Mean gap", "Top 10 gap")) %>%
  mutate(juniors = ifelse(juniors, "With juniors", "W/o juniors")) %>%
  mutate(inactives = ifelse(inactives, "with inactives", "w/o inactives")) %>%
  mutate(filter = fct_rev(str_c(juniors, ", ", inactives)), .before = 1) %>%
  mutate(floor = str_c("Rating floor: ", floor)) %>%
  ggplot(aes(x = `M`, y = `F`, color = filter)) +
  geom_abline(aes(slope = 1, intercept = 0), linetype = "dashed", color = "grey40") +
  geom_point(size = 0.8, alpha = 0.6) +
  facet_grid(metric ~ floor, scales = "free") +
  scale_color_viridis_d(name = "", option = "C", end = 0.85, direction = -1) +
  theme_bw() +
  theme(panel.grid = element_blank())

library(tidyverse)


federations <- function(rating_data, min_players) {
  participation_gap(rating_data) %>%
    mutate(no_minority = pmin(`F`, `M`)) %>%
    filter(no_minority >= min_players) %>%
    pull(fed)
}


top10 <- function(x) mean(tail(sort(x), 10L))


top1 <- max


rating_data <- read_csv("data/rating-data.csv", col_types = "cccdiil")


tibble(cond = list(all  = \(g) g > 0,
                   med  = \(g) g >= 350 & g < 400,
                   high = \(g) g >= 750 & g < 800)) %>%
  crossing(metric = list(mean = mean, top10 = top10, top1 = top1)) %>%
  mutate(cond_name = names(cond), metric_name = names(metric)) %>%
  mutate(data = map2(cond, metric, \(cond, metric) {
    rating_data %>%
      filter(fed %in% federations(filter(rating_data, cond(games)), 1)) %>%
      filter(cond(games)) %>%
      filter(active) %>%
      select(fed | sex | rating) %>%
      pivot_wider(names_from = sex, values_from = rating,
                  values_fn = list, names_prefix = "ratings_") %>%
      mutate(`M` = map_int(ratings_M, length),
             `F` = map_int(ratings_F, length)) %>%
      filter(`M` > 0 & `F` > 0) %>%
      mutate(frac_F = `F` / (`F` + `M`)) %>%
      mutate(ratings_F = map_dbl(ratings_F, metric),
             ratings_M = map_dbl(ratings_M, metric)) %>%
      mutate(diff = ratings_M - ratings_F)
  } )) %>%
  unnest(data) %>%
  ggplot(aes(x = frac_F, y = diff)) +
  geom_point(colour = viridis::plasma(1)) +
  geom_smooth(method = lm, se = FALSE, colour = viridis::plasma(1, begin = 0.5)) +
  facet_grid(cond_name ~ metric_name, scales = "free") +
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

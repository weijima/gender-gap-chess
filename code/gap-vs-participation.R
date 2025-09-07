library(tidyverse)


participation_gap <- function(rating_data) {
  rating_data %>%
    count(fed, sex, name = "no_of_players") %>%
    pivot_wider(names_from = "sex", values_from = "no_of_players", values_fill = 0) %>%
    mutate(frac_W = `F` / (`F` + `M`))
}


federations <- function(rating_data, min_players) {
  participation_gap(rating_data) %>%
    mutate(no_minority = pmin(`F`, `M`)) %>%
    filter(no_minority >= min_players) %>%
    pull(fed)
}


restrict_data <- function(rating_data, include_junior, include_inactive, min_rating,
                          min_players = 30, birth_uncertain = FALSE) {
  if (include_junior) max_byear <- 2019 else max_byear <- 1999
  rating_data %>%
    filter(if (include_inactive) TRUE else active) %>%
    filter(if (birth_uncertain) TRUE else !is.na(born)) %>%
    filter(born <= max_byear | is.na(born), rating >= min_rating) %>%
    filter(fed %in% federations(., min_players))
}


rating_data <- read_csv("data/rating-data.csv", col_types = "cccdiil")

null_data <- read_csv("data/null-stats.csv", col_types = "llicccd")



joint_dat <-
  null_data %>%
  filter(fed != "ALL" & stat == "obs") %>%
  select(!stat) %>%
  filter(metric %in% c("mean", "top10", "top1")) %>%
  left_join(
    crossing(juniors = c(FALSE, TRUE),
             inactives = c(FALSE, TRUE),
             floor = c(1000, 1400, 1600)) %>%
      mutate(data = pmap(., \(juniors, inactives, floor) {
        restrict_data(rating_data, juniors, inactives, floor, 30) %>%
          participation_gap()
      } )) %>%
      unnest(data),
    by = join_by(juniors, inactives, floor, fed)
  )


joint_dat %>%
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
  nest(data = !metric) %>%
  mutate(metric_lab = as_factor(case_when(
    metric == "mean"  ~ "overall mean gap",
    metric == "top10" ~ "top 10 gap",
    metric == "top1"  ~ "top 1 gap"
  ))) %>%
  mutate(plot = map2(metric_lab, data, \(metric_lab, data) {
    ggplot(data, aes(x = frac_W, y = value)) +
      geom_point(color = viridis::plasma(1)) +
      geom_smooth(method = lm, se = FALSE, alpha = 0.3,
                  color = viridis::plasma(1, begin = 0.5)) +
      facet_grid(floor_txt ~ filter, scales = "free_y") +
      scale_x_continuous(breaks = c(0, 0.15, 0.3), labels = scales::percent) +
      labs(x = "Proportion of women per federation",
           y = str_c("Average ", metric_lab, " per federation")) +
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
  })) %>%
  mutate(metric = walk2(metric, plot, \(metric, plot) {
    ggsave(str_c("figures/particip-", metric, ".pdf"), plot, width = 6, height = 4.5)
  } ))


joint_dat %>%
  select(!`F` & !`M`) %>%
  nest(data = fed | value | frac_W) %>%
  mutate(fit = map(data, \(x) broom::tidy(lm(value ~ frac_W, data = x)))) %>%
  unnest(fit) %>%
  filter(term == "frac_W") %>%
  select(!data & !term) %>%
  filter(estimate >= 0) %>%
  arrange(p.value)

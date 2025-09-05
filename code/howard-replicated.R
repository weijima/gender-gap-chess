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
  mutate(class = case_when(
    frac_W <= 0.05                  ~ "5% or fewer women",
    frac_W >= 0.25                  ~ "25% or more women",
    frac_W >  0.05 & frac_W <  0.25 ~ "5-25% women",
    TRUE                            ~ NA
  )) %>%
  drop_na() %>%
  summarize(
    mean_diff = mean(value),
    sd_diff = sd(value),
    N = n(),
    SEM = sd_diff / sqrt(N),
    CI = SEM * qt(1 - 0.025, N - 1),
    .by = c(juniors, inactives, floor, metric, class)
  ) %>%
  filter(metric == "top10") %>%
  mutate(class = fct_relevel(class, "5% or fewer women", "5-25% women")) %>%
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
  ggplot(aes(x = class, y = mean_diff,
             ymin = mean_diff - CI, ymax = mean_diff + CI, group = 0)) +
  geom_point(color = viridis::plasma(1)) +
  geom_errorbar(width = 0.2, color = viridis::plasma(1)) +
  geom_line(alpha = 0.3, color = viridis::plasma(1)) +
  facet_grid(filter ~ floor_txt) +
  labs(x = NULL, y = "Mean of rating differences") +
  theme_bw()



joint_dat %>%
  filter(metric == "top1") %>%
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
  ggplot(aes(x = frac_W, y = value)) +
  geom_point(color = viridis::plasma(1)) +
  geom_smooth(method = lm, se = FALSE, alpha = 0.3,
              color = viridis::plasma(1, begin = 0.5)) +
  facet_grid(filter ~ floor_txt) +
  labs(x = NULL, y = "Mean of rating differences") +
  theme_bw()

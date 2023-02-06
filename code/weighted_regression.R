library(tidyverse)
library(ggfortify)


participation_gap <- function(rating_data) {
  rating_data %>%
    count(fed, sex, name = "no_of_players") %>%
    pivot_wider(names_from = "sex", values_from = "no_of_players") %>%
    replace_na(list(`F` = 0, `M` = 0)) %>%
    mutate(participation_gap = `M` / (`F` + `M`))
}

federations <- function(rating_data, min_players) {
  participation_gap(rating_data) %>%
    mutate(no_minority = pmin(`F`, `M`)) %>%
    filter(no_minority >= min_players) %>%
    pull(fed)
}

restrict_data <- function(rating_data, max_byear, min_rating, min_players = 30,
                          include_inactive = FALSE, birth_uncertain = FALSE) {
  rating_data %>%
    filter(if (include_inactive) TRUE else active) %>%
    filter(if (birth_uncertain) TRUE else born != 0) %>%
    filter(born <= max_byear, rating >= min_rating) %>%
    filter(fed %in% federations(., min_players))
}

diff_table <- function(rating_data, fun = mean, max_byear = 2019,
                       min_rating = 1000, min_players = 30,
                       include_inactive = FALSE, birth_uncertain = FALSE) {
  rating_data %>%
    restrict_data(max_byear, min_rating, min_players,
                  include_inactive, birth_uncertain) %>%
    group_by(fed, sex) %>%
    summarise(rating = fun(rating), games = fun(games), age = fun(born)) %>%
    ungroup() %>%
    pivot_wider(names_from = sex, values_from = c(rating, games, age)) %>%
    transmute(fed, y = rating_F - rating_M, E = games_F - games_M, A = age_F - age_M)
}

wreg <- function(rating_data, null_data, fun = mean, max_byear = 2019,
                 min_rating = 1000, min_players = 30,
                 include_inactive = FALSE, birth_uncertain = FALSE) {
  diff_table(rating_data, fun, max_byear, min_rating, min_players,
             include_inactive, birth_uncertain) %>%
    left_join(null_data, by = "fed") %>%
    transmute(fed, ym = y - mean, E, A, weights = 1 / sd^2) %>%
    lm(ym ~ E + A, data = ., weights = weights)
}

adjusted_data <- function(rating_data, null_data, fun = mean, max_byear = 2019,
                          min_rating = 1000, min_players = 30,
                          include_inactive = FALSE, birth_uncertain = FALSE) {
  difftab <- diff_table(rating_data, fun, max_byear, min_rating, min_players,
                        include_inactive, birth_uncertain) %>%
    left_join(null_data, by = "fed") %>%
    transmute(fed, ym = y - mean, E, A, weights = 1 / sd^2)
  fit <- lm(ym ~ E + A, data = difftab, weights = weights)
  wE <- coef(fit)["E"]
  wA <- coef(fit)["A"]
  difftab %>% transmute(fed, yPEA = ym - wE*E - wA*A)
}

rating_data <- read_rds("../data/rating_data.rds")

null_data <- read_csv("../data/nulls/nulls-J1-I0.csv", show_col_types = FALSE) %>%
  rename_with(tolower) %>%
  select(fed, mean_ptmean, mean_ptsd) %>%
  rename(mean = mean_ptmean, sd = mean_ptsd)


participation_gap(rating_data) %>%
  select(fed, participation_gap) %>%
  inner_join(
    adjusted_data(rating_data, null_data, max_byear = 2019, min_rating = 1000),
    by = "fed"
  ) %>%
  ggplot(aes(x = participation_gap, y = yPEA, label = fed)) +
  geom_text(fontface = "bold", colour = "firebrick", alpha = 0.6) +
  theme_bw()

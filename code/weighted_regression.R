library(tidyverse)
library(ggfortify)


diagplot <- function(modelFit, which = 3:2, smooth.colour = NA,
                     alpha = 0.8, colour = "steelblue") {
  autoplot(modelFit, which = which, smooth.colour = NA,
           alpha = alpha, colour = colour) +
    theme_bw() +
    theme(panel.grid = element_blank())
}

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

diff_table <- function(rating_data, fun = mean) {
  rating_data %>%
    group_by(fed, sex) %>%
    summarise(rating = fun(rating), games = fun(games), age = fun(born),
              .groups = "drop") %>%
    pivot_wider(names_from = sex, values_from = c(rating, games, age)) %>%
    transmute(fed, y = rating_M - rating_F, E = games_M - games_F, A = age_M - age_F)
}

wreg <- function(rating_data, null_data, fun = mean) {
  diff_table(rating_data, fun) %>%
    left_join(null_data, by = "fed") %>%
    transmute(fed, ym = y - mean, E, A, weights = 1 / sd^2) %>%
    lm(ym ~ E + A, data = ., weights = weights)
}

adjusted_data <- function(rating_data, null_data, fun = mean) {
  difftab <- diff_table(rating_data, fun) %>%
    left_join(null_data, by = "fed") %>%
    transmute(fed, ym = y - mean, E, A, weights = 1 / sd^2)
  fit <- lm(ym ~ E + A, data = difftab, weights = weights)
  wE <- coef(fit)["E"]
  wA <- coef(fit)["A"]
  difftab %>% transmute(fed, yPEA = ym - wE*E - wA*A)
}

restrict_null <- function(null_tab, metric_mean, metric_sd, include_junior = TRUE,
                          include_inactive = FALSE, min_rating = 1000) {
  null_tab %>%
    filter(include_junior == {{include_junior}},
           include_inactive == {{include_inactive}},
           rating_floor == min_rating,
           metric %in% c(metric_mean, metric_sd)) %>%
    select(fed, metric, value) %>%
    pivot_wider(names_from = metric, values_from = value) %>%
    rename(mean = all_of(metric_mean), sd = all_of(metric_sd))
}


rating_data <- read_rds("../data/rating_data.rds")

null_data <- read_rds("../data/nulls/nulls.rds")

# rating_data %>%
#   restrict_data(max_byear = 2019, min_rating = 1000, min_players = 30,
#                 include_inactive = FALSE, birth_uncertain = FALSE) %>%
#   adjusted_data(restrict_null(null_data, "mean_ptmean", "mean_ptsd",
#                               include_junior = TRUE, include_inactive = FALSE,
#                               min_rating = 1000),
#                 fun = function(x) mean(tail(sort(x), 10))) %>%
#   arrange(yPEA) %>%
#   mutate(fed = fct_rev(as_factor(fed))) %>%
#   ggplot(aes(x = yPEA, y = fed)) +
#   geom_col(colour = "steelblue", fill = "steelblue", alpha = 0.2) +
#   labs(x = "Adjusted rating gap", y = "Federation") +
#   theme_bw() +
#   theme(panel.grid = element_blank())

tibble(fun = list(mean = mean,
                  top10 = function(x) mean(tail(sort(x), 10)),
                  top1 = max)) %>%
  mutate(metric = names(fun)) %>%
  mutate(fit = map(fun, ~restrict_data(rating_data,
                                       max_byear = 2019,
                                       min_rating = 1000,
                                       min_players = 30,
                                       include_inactive = FALSE,
                                       birth_uncertain = FALSE) %>%
                     wreg(restrict_null(null_data,
                                        "max10_ptmean",
                                        "max10_ptsd",
                                        include_junior = TRUE,
                                        include_inactive = FALSE,
                                        min_rating = 1000),
                          fun = .x))) %>%
  mutate(summary = map(fit, compose(broom::tidy, summary))) %>%
  mutate(quality = map(fit, ~broom::glance(.x) %>% select("r.squared"))) %>%
  #pull(fit) %>% map(diagplot)
  unnest(c(summary, quality)) %>% select(-c(fun, fit, statistic))

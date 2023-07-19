library(tidyverse)

top1 <- max

top10 <- function(x) mean(tail(sort(x), 10))

participation_gap <- function(rating_data) {
  rating_data %>%
    count(fed, sex, name = "no_of_players") %>%
    pivot_wider(names_from = "sex", values_from = "no_of_players", values_fill = 0) %>%
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
    filter(if (birth_uncertain) TRUE else !is.na(born)) %>%
    filter(born <= max_byear | is.na(born), rating >= min_rating) %>%
    filter(fed %in% federations(., min_players))
}

restrict_null <- function(null_tab, metr, include_junior = TRUE,
                          include_inactive = FALSE, min_rating = 1000) {
  null_tab %>%
    filter(juniors == include_junior,
           inactives == include_inactive,
           floor == min_rating,
           metric == metr,
           stat %in% c("ptmean", "ptsd")) %>%
    select(fed, stat, value) %>%
    pivot_wider(names_from = stat, values_from = value) %>%
    rename(mean = ptmean, sd = ptsd)
}

diff_table <- function(rating_data, .f = mean) {
  rating_data %>%
    mutate(age = 2019 - born) %>%
    arrange(fed, sex, desc(rating)) %>%
    group_by(fed, sex) %>%
    slice_head(n = { if (identical(.f, top10)) 10 else
      if (identical(.f, top1)) 1 else nrow(.) }) %>%
    summarise(rating=mean(rating), games=mean(games), age=mean(age), .groups="drop") %>%
    pivot_wider(names_from = sex, values_from = c(rating, games, age)) %>%
    transmute(fed, y = rating_M - rating_F, E = games_M - games_F, A = age_M - age_F)
}

wreg <- function(rating_data, null_data, .f = mean) {
  diff_table(rating_data, .f) %>%
    inner_join(null_data, by = "fed") %>%
    transmute(fed, yP = y - mean, E, A, weight = 1 / sd^2) %>%
    lm(yP ~ E + A, data = ., weights = weight)
}

adjusted_data <- function(rating_data, null_data, .f = mean) {
  difftab <- diff_table(rating_data, .f) %>%
    inner_join(null_data, by = "fed") %>%
    transmute(fed, yP = y - mean, E, A, weight = 1 / sd^2)
  fit <- lm(yP ~ E + A, data = difftab, weights = weight)
  wE <- coef(fit)["E"]
  wA <- coef(fit)["A"]
  difftab %>% mutate(fed, yPEA = yP - wE*E - wA*A)
}

analyze_age_experience <- function(rating_data, null_data, min_rating = 1000,
                                   min_players = 30, include_junior = TRUE,
                                   include_inactive = FALSE, birth_uncertain = FALSE) {
  if (include_junior) max_byear <- 2019 else max_byear <- 1999
  tibble(fun = list(mean = mean, top10 = top10, top1 = top1)) %>%
    mutate(metric = names(fun)) %>%
    mutate(fit = map2(fun, metric,
                      ~restrict_data(rating_data,
                                     max_byear = max_byear,
                                     min_rating = min_rating,
                                     min_players = min_players,
                                     include_inactive = include_inactive,
                                     birth_uncertain = birth_uncertain) %>%
                        wreg(restrict_null(null_data,
                                           metr = .y,
                                           include_junior = include_junior,
                                           include_inactive = include_inactive,
                                           min_rating = min_rating),
                             .f = .x))) %>%
    mutate(summary = map(fit, compose(broom::tidy, summary))) %>%
    mutate(quality = map(fit, ~broom::glance(.x) %>% select("r.squared"))) %>%
    unnest(c(summary, quality)) %>%
    select(metric, term, estimate, std.error, p.value, r.squared)
}


rating_data <- read_csv("data/rating-data.csv", col_types = "cccdiil")

null_data <- read_csv("data/null-stats.csv", show_col_types = FALSE) %>%
  filter(fed != "ALL")


# Perform weighted regression, with a given set of parameters:
analyze_age_experience(rating_data, null_data, min_rating = 1000, min_players = 30,
                       include_junior = TRUE, include_inactive = FALSE,
                       birth_uncertain = FALSE)

# Table of coefficients, weights, and regression results for all parameter combinations:
crossing(fun = list(mean = mean, top10 = top10, top1 = top1),
         juniors = c(TRUE, FALSE),
         inactives = c(TRUE, FALSE),
         floor = c(1000, 1400, 1600)) %>%
  mutate(metric = names(fun), fun = unname(fun)) %>%
  mutate(tab = pmap(list(fun, juniors, inactives, floor, metric),
                    function(fun, juniors, inactives, floor, metric) {
                      rating_data %>%
                        restrict_data(max_byear = ifelse(juniors, 2019, 1999),
                                      min_rating = floor,
                                      min_players = 30,
                                      include_inactive = inactives,
                                      birth_uncertain = FALSE) %>%
                        adjusted_data(restrict_null(null_data,
                                                    metr = metric,
                                                    include_junior = juniors,
                                                    include_inactive = inactives,
                                                    min_rating = floor),
                                      .f = fun)
                    })) %>%
  unnest(tab) %>%
  select(metric, juniors, inactives, floor, fed, yP, yPEA, E, A, weight) %>%
  write_csv("data/age-experience-tab.csv")

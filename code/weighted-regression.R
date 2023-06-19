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

diff_table <- function(rating_data, .f = mean) {
  rating_data %>%
    mutate(age = 2019 - born) %>%
    arrange(fed, sex, desc(rating)) %>%
    group_by(fed, sex) %>%
    slice_head(n = { if (identical(.f, top10)) 10 else
      if (identical(.f, top1)) 1 else nrow(.) }) %>%
    summarise(rating=mean(rating), games=mean(games), age=mean(age), .groups="drop") %>%
    ungroup() %>%
    pivot_wider(names_from = sex, values_from = c(rating, games, age)) %>%
    transmute(fed, y = rating_M - rating_F, E = games_M - games_F, A = age_M - age_F)
}

wreg <- function(rating_data, null_data, .f = mean) {
  diff_table(rating_data, .f) %>%
    inner_join(null_data, by = "fed") %>%
    transmute(fed, yP = y - mean, E, A, weights = 1 / sd^2) %>%
    lm(yP ~ E + A, data = ., weights = weights)
}

wreg_age <- function(rating_data, null_data, .f = mean) {
  diff_table(rating_data, .f) %>%
    inner_join(null_data, by = "fed") %>%
    transmute(fed, yP = y - mean, A, weights = 1 / sd^2) %>%
    lm(yP ~ A, data = ., weights = weights)
}

adjusted_data <- function(rating_data, null_data, .f = mean) {
  difftab <- diff_table(rating_data, .f) %>%
    inner_join(null_data, by = "fed") %>%
    transmute(fed, yP = y - mean, E, A, weights = 1 / sd^2)
  fit <- lm(yP ~ E + A, data = difftab, weights = weights)
  wE <- coef(fit)["E"]
  wA <- coef(fit)["A"]
  difftab %>% mutate(fed, yPEA = yP - wE*E - wA*A)
}

adjusted_data_age <- function(rating_data, null_data, .f = mean) {
  difftab <- diff_table(rating_data, .f) %>%
    inner_join(null_data, by = "fed") %>%
    transmute(fed, yP = y - mean, A, weights = 1 / sd^2)
  fit <- lm(yP ~ A, data = difftab, weights = weights)
  wA <- coef(fit)["A"]
  difftab %>% mutate(fed, yPA = yP - wA*A)
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

top10 <- function(x) mean(tail(sort(x), 10))

top1 <- max

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
    #pull(fit) %>% map(diagplot)
    unnest(c(summary, quality)) %>%
    select(metric, term, estimate, std.error, p.value, r.squared)
}


rating_data <- read_csv("data/rating-data.csv", col_types = "cccdiil")

null_data <- read_csv("data/null-stats.csv") %>%
  # Adjust values for different conventions (F - M to M - F):
  mutate(value = case_when(
    stat %in% c("obs", "ptmean") ~ -value,
    stat == "ptpval" ~ 1 - value,
    .default = value
  ))

analyze_age_experience(rating_data, null_data, min_rating = 1000, min_players = 30,
                       include_junior = TRUE, include_inactive = FALSE,
                       birth_uncertain = FALSE)


# tibble(fun = list(mean = mean,
#                   top10 = top10,
#                   top1 = top1)) %>%
#   mutate(metric = names(fun)) %>%
#   mutate(tab = map2(fun, metric,
#                     ~restrict_data(rating_data,
#                                    max_byear = 2019,
#                                    min_rating = 1000,
#                                    min_players = 30,
#                                    include_inactive = FALSE,
#                                    birth_uncertain = FALSE) %>%
#                       adjusted_data_age(restrict_null(null_data,
#                                                       metr = .y,
#                                                       include_junior = TRUE,
#                                                       include_inactive = FALSE,
#                                                       min_rating = 1000),
#                                         .f = .x))) %>%
#   mutate(fit = map2(fun, metric,
#                    ~restrict_data(rating_data,
#                                   max_byear = 2019,
#                                   min_rating = 1000,
#                                   min_players = 30,
#                                   include_inactive = FALSE,
#                                   birth_uncertain = FALSE) %>%
#                      wreg_age(restrict_null(null_data,
#                                             metr = .y,
#                                             include_junior = TRUE,
#                                             include_inactive = FALSE,
#                                             min_rating = 1000),
#                               .f = .x))) %>%
#   mutate(summary = map(fit, compose(broom::tidy, summary)),
#          quality = map(fit, ~broom::glance(.x) %>% select("r.squared"))) %>%
#   #unnest(c(summary, quality)) %>% select(-fun, -tab, -fit)
#   mutate(plot = map2(tab, summary, ~ggplot(data = .x, aes(x = A, y = yP)) +
#                        geom_point(colour = "steelblue") +
#                        #geom_smooth(colour = "black", se = FALSE, method = lm) +
#                        geom_abline(data = .y %>%
#                                      select(term, estimate) %>%
#                                      pivot_wider(names_from = "term",
#                                                  values_from = "estimate"),
#                                    aes(intercept=`(Intercept)`, slope=A)) +
#                        geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5) +
#                        theme_bw() +
#                        theme(panel.grid = element_blank()))) %>%
#   pull(plot) %>%
#   cowplot::plot_grid(plotlist = ., nrow = 1)


crossing(fun = list(mean = mean, top10 = top10, top1 = top1),
         juniors = c(TRUE, FALSE),
         rating_floor = c(1000, 1400, 1600)) %>%
  mutate(metric = names(fun)) %>%
  mutate(tab = pmap(list(fun, juniors, rating_floor, metric),
                    ~rating_data %>%
                      restrict_data(max_byear = ifelse(..2, 2019, 1999),
                                    min_rating = ..3,
                                    min_players = 30,
                                    include_inactive = FALSE,
                                    birth_uncertain = FALSE) %>%
                      adjusted_data(restrict_null(null_data,
                                                  metr = ..4,
                                                  include_junior = ..2,
                                                  include_inactive = FALSE,
                                                  min_rating = ..3),
                                    .f = ..1))) %>%
  unnest(tab) %>%
  select(metric, juniors, rating_floor, fed, yP, yPEA, E, A, weights) %>%
  write_csv("data/age-experience-tab.csv")

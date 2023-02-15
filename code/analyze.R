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

compare <- function(rating_data, colname, test = permut_test) {
  rating_data %>%
    select(fed, sex, rating) %>%
    pivot_wider(names_from = sex, values_from = rating, values_fn = list) %>%
    transmute(fed, "{{colname}}" := map2_dbl(`F`, `M`, test))
}

permut_test <- function(x, y, fun = mean, perms = 1000, plot = FALSE) {
  nx <- length(x)
  ny <- length(y)
  n <- nx + ny
  pool <- c(x, y)
  observed <- fun(x) - fun(y)
  diffs <- rep(0, times = perms)
  for (i in 1:perms) {
    permut <- sample(pool)
    diffs[i] <- fun(permut[1:nx]) - fun(permut[(nx + 1):n])
  }
  if (plot) {
    hist(diffs, breaks = 30, xlim = c(min(diffs, observed), max(diffs, observed)))
    abline(v = observed, col = "red")
  }
  return(mean(observed <= diffs))
}

gap_chart <- function(gap_data, signif = 0.001) {
  gap_data %>%
    mutate(rating_gap = case_when(
      pvalue < signif ~ "female-slanted",
      pvalue > 1 - signif ~ "male-slanted",
      TRUE ~ "nonsignificant"
    )) %>%
    ggplot(aes(x = participation_gap, y = diff, label = fed, colour = rating_gap)) +
    geom_text(fontface = "bold", alpha = 0.7) +
    scale_x_continuous(name = "participation gap", labels = scales::percent) +
    scale_y_continuous(name = "rating difference") +
    scale_color_manual(values = c("blue4", "gray55", "firebrick"),
                       name = "rating gap",
                       limits = c("female-slanted", "nonsignificant", "male-slanted")) +
    theme_bw()
}

analyze_ratings <- function(rating_data, fn = mean, perms = 1000, test = permut_test) {
  rating_data %>%
    compare(pvalue, test = function(f, m) test(f, m, fn, perms)) %>%
    left_join(participation_gap(rating_data), by = "fed") %>%
    left_join(compare(rating_data, diff, function(f, m) fn(f) - fn(m)), by = "fed")
}

restrict_data <- function(rating_data, max_byear = 1999, min_rating = 1400,
                          min_players = 30, include_inactive = FALSE,
                          birth_uncertain = FALSE) {
  rating_data %>%
    filter(if (include_inactive) TRUE else active) %>%
    filter(if (birth_uncertain) TRUE else born != 0) %>%
    filter(born <= max_byear, rating >= min_rating) %>%
    filter(fed %in% federations(., min_players))
}

linearModel <- function(data, formula, print = FALSE, plot = FALSE, ...) {
  fit <- lm(formula, data)
  if (print) print(anova(fit))
  if (plot) suppressWarnings(show(autoplot(fit, ...)))
  fit
}

experienceByFed <- function(rating_data, model, max_byear = 1999, min_rating = 1400,
                            min_players = 30, include_inactive = FALSE,
                            birth_uncertain = FALSE, ...) {
  rating_data %>%
    restrict_data(max_byear, min_rating, min_players,
                  include_inactive, birth_uncertain) %>%
    group_by(fed, sex) %>%
    summarise(rating = mean(rating), games = mean(games), age = mean(born),
              age2 = age^2, .groups = "drop") %>%
    linearModel(model, ...)
}


# rating_data <- read_rds("../data/fideratings.rds") %>%
#   rename(id=fideid, fed=country, games=numgames, born=dob_yr, sex=sex_mode) %>%
#   mutate(id = as.character(id)) %>%
#   mutate(active = str_detect(flag, "i")) %>%
#   mutate(year = as.integer(lubridate::year(listdate))) %>%
#   mutate(month = as.integer(lubridate::month(listdate))) %>%
#   select(-c(name1, flag, rtgchange, listdate, title, k))


rating_data <- read_rds("../data/rating_data.rds")

analysis <- rating_data %>%
  restrict_data() %>%
  analyze_ratings(function(x) mean(x), perms = 5000, test = function(x, y, fn, perms)
    wilcox.test(x, y, alternative = "greater")$p.value)


with(
  tibble(signif = 0.001),
  { show(gap_chart(analysis, signif))
    analysis %>%
      filter(pvalue < signif | pvalue > 1 - signif) %>%
      arrange(participation_gap) %>%
      print(n = Inf)
  }
)


# Regression to see effects of sex and experience
experienceByFed(rating_data, model = rating ~ sex + games + age + age2,
                max_byear = 2019, min_rating = 0, min_players = 30, print = TRUE,
                plot = TRUE, smooth.colour = NA, which = 1:3, alpha = 0.3)

modelDat <- crossing(max_byear = seq(1990, 2018, by = 1),
                     min_rating = seq(1000, 1600, by = 50)) %>%
  mutate(fit = map2(max_byear, min_rating,
                    ~experienceByFed(rating_data, rating ~ sex + games + age + age2,
                                     max_byear = .x, min_rating = .y))) %>%
  mutate(anova = map(fit, compose(broom::tidy, anova)),
         summary = map(fit, compose(broom::tidy, summary)),
         quality = map(fit, broom::glance))

modelDat %>%
  unnest(anova) %>%
  mutate(term = recode(term, "games" = "experience", "sex" = "gender",
                       "age2" = "age squared")) %>%
  filter(!is.na(p.value), term != "age squared") %>%
  mutate(term = fct_relevel(term, "experience", "gender")) %>%
  mutate(p.value = ifelse(p.value > 0.05, NA, p.value)) %>%
  ggplot(aes(x = max_byear, y = min_rating, fill = log10(p.value))) +
  geom_raster() +
  facet_wrap(~ term) +
  scale_x_continuous(name = "maximum year of birth", expand = c(0, 0)) +
  scale_y_continuous(name = "minimum rating", expand = c(0, 0)) +
  scale_fill_continuous(name = expression(paste(log[10](p)))) +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank())

modelDat %>%
  mutate(quality = map(quality, ~select(.x, -p.value, -statistic))) %>%
  unnest(c(summary, quality)) %>%
  mutate(term = recode(term, "games" = "experience", "sexM" = "gender",
                       "age2" = "age squared")) %>%
  mutate(term = fct_relevel(term, "experience", "gender")) %>%
  filter(!is.na(p.value), term == "gender") %>%
  mutate(p.value = ifelse(p.value > 0.05, NA, p.value)) %>%
  mutate(estimate = estimate * (p.value > 0)) %>%
  ggplot(aes(x = max_byear, y = min_rating, fill = estimate)) +
  geom_raster() +
  facet_wrap(~ term) +
  scale_x_continuous(name = "maximum year of birth", expand = c(0, 0)) +
  scale_y_continuous(name = "minimum rating", expand = c(0, 0)) +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank())

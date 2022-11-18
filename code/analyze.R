library(tidyverse)
#library(Rcpp)


#sourceCpp("permutation_tests.cpp")

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
  observed_diff <- abs(fun(x) - fun(y))
  diffs <- rep(0, times = perms)
  for (i in 1:perms) {
    permut <- sample(pool)
    diffs[i] <- abs(fun(permut[1:nx]) - fun(permut[(nx + 1):n]))
  }
  if (plot) {
    hist(diffs, breaks = 30, xlim = range(diffs) + c(0, observed_diff))
    abline(v = observed_diff, col = "red")
  }
  return(mean(observed_diff <= diffs))
}

gap_chart <- function(gap_data, signif = 0.001) {
  ymax <- max(abs(gap_data$diff))
  gap_data %>%
    mutate(rating_gap = if_else(pvalue < signif, "significant", "nonsignificant")) %>%
    mutate(rating_gap = fct_relevel(rating_gap, "significant")) %>%
    ggplot(aes(x = participation_gap, y = diff, label = fed, colour = rating_gap)) +
    geom_text(fontface = "bold", alpha = 0.7) +
    geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.4) +
    scale_x_continuous(name = "participation gap", labels = scales::percent) +
    scale_y_continuous(name = "rating difference", limits = c(-ymax, ymax)) +
    scale_colour_manual(values = c("firebrick", "gray55"), name = "rating gap") +
    theme_bw()
}

analyze_ratings <- function(rating_data, fn = mean, signif = 0.001, perms = 1000,
                            plot = TRUE) {
  rating_data %>%
    compare(pvalue, test = function(f, m) permut_test(f, m, fn, perms)) %>%
    left_join(participation_gap(rating_data), by = "fed") %>%
    left_join(compare(rating_data, diff, function(f, m) fn(f) - fn(m)), by = "fed") %>%
    { if (plot) show(gap_chart(.)); . } %>%
    filter(pvalue < signif) %>%
    arrange(participation_gap)
}

restrict_data <- function(rating_data, max_byear = 1999, min_rating = 1400,
                          min_players = 30, include_inactive = FALSE,
                          birth_uncertain = FALSE) {
  rating_data %>%
    filter(if (include_inactive) active else TRUE) %>%
    filter(if (birth_uncertain) born != 0 else TRUE) %>%
    filter(born <= max_byear, rating >= min_rating, active) %>%
    filter(fed %in% federations(., min_players))
}


rating_data <- read_rds("../data/rating_data.rds")

rating_data %>%
  restrict_data() %>%
  analyze_ratings(function(x) mean(sort(x, decreasing = TRUE)[1:10]), perms = 1000) %>%
  print(n = Inf)

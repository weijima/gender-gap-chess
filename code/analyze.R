library(tidyverse)
library(Rcpp)


sourceCpp("permutation_tests.cpp")

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

compare <- function(rating_data, colname, test = function(f, m) ks.test(f, m)$p.value) {
  rating_data %>%
    select(fed, sex, rating) %>%
    pivot_wider(names_from = sex, values_from = rating, values_fn = list) %>%
    transmute(fed, "{{colname}}" := map2_dbl(`F`, `M`, test))
}

permut_test <- function(x, y, perms = 10000, plot = FALSE) {
  nx <- length(x)
  ny <- length(y)
  pool <- c(x, y)
  observed_diff <- abs(mean(x) - mean(y))
  diffs <- rep(0, times = perms)
  for (i in 1:length(diffs)) {
    diffs[i] <- abs(mean(sample(pool, size = nx)) - mean(sample(pool, size = ny)))
  }
  if (plot) { hist(diffs, breaks = 30); abline(v = observed_diff, col = "red") }
  list(p.value = mean(diffs >= observed_diff))
}

gap_chart <- function(gap_data, signif = 0.001, ylab = "rating difference") {
  ymax <- max(abs(gap_data$diff))
  gap_data %>%
    mutate(rating_gap = if_else(pvalue < signif, "significant", "nonsignificant")) %>%
    mutate(rating_gap = fct_relevel(rating_gap, "significant")) %>%
    ggplot(aes(x = participation_gap, y = diff, label = fed, colour = rating_gap)) +
    geom_text(fontface = "bold", alpha = 0.7) +
    geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.4) +
    scale_x_continuous(name = "participation gap", labels = scales::percent) +
    scale_y_continuous(name = ylab, limits = c(-ymax, ymax)) +
    scale_colour_manual(values = c("firebrick", "gray55"), name = "rating gap") +
    theme_bw()
}


rating_data <- read_rds("../data/rating_data.rds") %>%
  filter(active, born != 0, born <= 1999, rating >= 1400) %>%
  filter(fed %in% federations(., 30))

rating_data %>%
  compare(pvalue, test = function(f, m) permtest(f, m, 5000)$p.value) %>%
  left_join(participation_gap(rating_data), by = "fed") %>%
  left_join(compare(rating_data, diff, function(f, m) mean(f) - mean(m)), by = "fed") %>%
  { show(gap_chart(.)); . } %>%
  filter(pvalue <= 0.001) %>%
  arrange(participation_gap) %>%
  print(n = Inf)

# this script has the current Figure 3.

library(tidyverse)
library(patchwork)


theme_set(theme_minimal() +
            theme(axis.line = element_line(colour = "grey80"),
                  axis.ticks = element_line(colour = "grey80"),
                  panel.grid = element_blank(),
                  panel.border = element_blank(),
                  panel.background = element_blank()))


ratings <- function(rating_data, jun, inact) {
  rating_data %>%
    filter(if (jun == "juniors") TRUE else age >= 20) %>%
    filter(if (inact == "inactives") TRUE else active)
}

nulls <- function(null_data = null_data, jun, inact, rating_floor = 1000) {
  null_data %>%
    filter(if (jun == "juniors") juniors else !juniors) %>%
    filter(if (inact == "inactives") inactives else !inactives) %>%
    filter(floor == rating_floor)
}


rating_data <- read_csv("data/rating-data.csv", show_col_types = FALSE) %>%
  filter(!is.na(born)) %>%
  mutate(age = year(as.Date("2019-01-01")) - born)

null_data <- read_csv("data/null-stats.csv", show_col_types = FALSE) %>%
  filter(fed != "ALL")

feds <- nulls(null_data, "juniors", "no_inactives") %>% pull(fed) %>% unique()




# Figure 1 ----------------------------------------------------------------------------
create_histogram <- function(data, bwidth = 100) {
  data %>%
    mutate(sex = ifelse(sex == "F", "Women", "Men")) %>%
    mutate(sex = fct_relevel(sex, "Women")) %>%
    ggplot(aes(x = rating)) +
    geom_histogram(aes(y = after_stat(density) * bwidth),
                   colour = "white", binwidth = bwidth) +
    facet_wrap(~ sex) +
    scale_x_continuous(limits = c(1000, 2900)) +
    labs(y = "Proportion", x = "Rating") +
    theme(axis.title = element_text(size = rel(0.8)))
}

w <- 5.4
h <- 3
create_histogram(ratings(rating_data, "juniors", "no_inactives"))
ggsave(file = "figures/fig_1_w_jun_no_ina.pdf", width = w, height = h)

create_histogram(ratings(rating_data, "juniors", "inactives"))
ggsave(file = "figures/fig_1_w_jun_w_ina.pdf", width = w, height = h)

create_histogram(ratings(rating_data, "no_juniors", "no_inactives"))
ggsave(file = "figures/fig_1_no_jun_no_ina.pdf", width = w, height = h)

create_histogram(ratings(rating_data, "no_juniors", "inactives"))
ggsave(file = "figures/fig_1_no_jun_w_ina.pdf", width = w, height = h)




# Figure 2 ----------------------------------------------------------------------------
participation_gap <- function(rating_data) {
  rating_data %>%
    count(fed, sex, name = "no_of_players") %>%
    pivot_wider(names_from = "sex", values_from = "no_of_players") %>%
    replace_na(list(`F` = 0, `M` = 0)) %>%
    transmute(fed, rel_F = `F` / (`F` + `M`))
}


get_summary_stats <- function(rating_data, feds_to_keep) {
  rating_data %>%
    filter(fed %in% feds_to_keep) %>%
    select(fed, sex, rating, age) %>%
    arrange(fed, sex, desc(rating)) %>%
    summarise(observed.ALL = mean(rating), age_obs.ALL = mean(age, na.rm = TRUE),
              observed.SD = sd(rating), age_obs.SD = mean(age, na.rm = TRUE),
              observed.MAX10 = mean(rating[1:10]), age_obs.MAX10 = mean(age[1:10]),
              observed.MAX1 = first(rating), age_obs.MAX1 = first(age),
              .by = c(fed, sex)) %>%
    left_join(participation_gap(rating_data), by = join_by(fed)) %>%
    pivot_longer(cols = contains(".")) %>%
    separate_wider_delim(cols = name, delim = ".", names = c("qty", "group")) %>%
    pivot_wider(names_from = qty, values_from = value)
}


reformat_results_data <- function(null_data, rating_data, feds_to_keep) {
  null_data %>%
    select(fed, metric, stat, value) %>%
    filter(metric != "median") %>%
    mutate(metric = case_match(metric, "mean" ~ "ALL", "sd" ~ "SD",
                               "top1" ~ "MAX1", "top10" ~ "MAX10")) %>%
    pivot_wider(names_from = stat, values_from = value) %>%
    inner_join(get_summary_stats(rating_data, feds_to_keep),
               by = join_by(fed, metric == group)) %>%
    rename(OBS_diff = obs)
}


get_lims <- function(plot_data, factor = 100) {
  ylims <- c(floor(min(plot_data$F) / factor) * factor,
             ceiling(max(plot_data$F) / factor) * factor)
  xlims <- c(floor(min(plot_data$M) / factor) * factor,
             ceiling(max(plot_data$M) / factor) * factor)
  c(min(ylims[1], xlims[1]), max(ylims[2], xlims[2]))
}


scatter_plot <- function(data, top = "ALL", lims = NULL) {
  plot_data <- data %>%
    filter(metric == top) %>%
    select(fed, sex, ptmean, ptpval, observed) %>%
    pivot_wider(values_from = observed, names_from = sex) %>%
    mutate(`*p*-value` = ifelse(ptpval < 0.05 | ptpval > 0.95,
                                "significant", "not significant"))
  if (is.null(lims)) lims <- get_lims(plot_data = plot_data)
  plot_data %>%
    ggplot(aes(y = `F`, x = `M`)) +
    geom_point(aes(colour = `*p*-value`), size = 0.8) +
    geom_abline(aes(slope = 1, intercept = 0), linetype = "dashed", color = "grey40") +
    coord_cartesian(xlim = lims, ylim = lims) +
    theme(legend.position = "none") +
    scale_color_manual(values = c("black", "tomato3"))
}


histogram_plot <- function(data, top, xlims = NULL) {
  data %>%
    filter(metric == top) %>%
    ggplot(aes(x = OBS_diff)) +
    geom_histogram(aes(y = after_stat(count / sum(count))),
                   colour = "white", fill = "grey50", bins = 30) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    expand_limits(y = c(0, 0.3), x = xlims) +
    geom_point(aes(x = mean(OBS_diff), y = -0.014), shape = 17, size = rel(1)) +
    scale_y_continuous(breaks = c(0, 0.1, 0.2, 0.3))
}


figure_2 <- function(data) {
  p11 <- scatter_plot(data, top = "ALL", lims = c(1200, 2900)) +
    labs(y = "Mean rating W", x = "Mean rating M")
  p12 <- scatter_plot(data, top = "MAX10", lims = c(1200, 2900)) +
    labs(y = "Mean rating W (Top 10)", x = "Mean rating M (Top 10)")
  p13 <- scatter_plot(data, top = "MAX1", lims = c(1200, 2900)) +
    labs(y = "Mean rating W (Top 1)", x = "Mean rating M (Top 1)")
  p14 <- scatter_plot(data, top = "SD") +
    labs(y = "SD rating W", x = "SD rating M")

  p21 <- histogram_plot(data, top = "ALL", xlims = c(0, 800)) +
    labs(x = "\u0394 mean rating", y = "Proportion\nof federations")
  p22 <- histogram_plot(data, top = "MAX10", xlims = c(0, 800)) +
    labs(x = "\u0394 mean rating (Top 10)", y = "Proportion\nof federations")
  p23 <- histogram_plot(data, top = "MAX1", xlims = c(0, 800)) +
    labs(x = "\u0394 rating (Top 1)", y = "Proportion\nof federations")
  p24 <- histogram_plot(data, top = "SD", xlims = c(0, 200)) +
    labs(x = "SD difference (men - women)", y = "Proportion\nof federations")

  (p11 + p21 + p12 + p22 + p13 + p23 + p14 + p24) +
    plot_layout(guides = "collect", design = "ACEG\nBDFH") +
    plot_annotation(tag_levels = "A") +
    theme(legend.position = 'bottom')  +
    theme(legend.title = ggtext::element_markdown())
}


reformat_results_data(nulls(null_data, "juniors", "no_inactives"),
                      ratings(rating_data, "juniors", "no_inactives"), feds) %>%
  figure_2()
#ggsave(file = "figures/fig_2_w_jun_no_ina.pdf", width = 8, height = 4)

reformat_results_data(nulls(null_data, "juniors", "inactives"),
                      ratings(rating_data, "juniors", "inactives"), feds) %>%
  figure_2()
#ggsave(file = "figures/fig_2_w_jun_w_ina.pdf", width = 8, height = 4)

reformat_results_data(nulls(null_data, "no_juniors", "no_inactives"),
                      ratings(rating_data, "no_juniors", "no_inactives"), feds) %>%
  figure_2()
#ggsave(file = "figures/fig_2_no_jun_no_ina.pdf", width = 8, height = 4)

reformat_results_data(nulls(null_data, "no_juniors", "inactives"),
                      ratings(rating_data, "no_juniors", "inactives"), feds) %>%
  figure_2()
#ggsave(file = "figures/fig_2_no_jun_w_ina.pdf", width = 8, height = 4)




# Figure 3 ----------------------------------------------------------------------------
adj_histogram_plot <- function(data, top, adj_var = "ptmean", xlims = c(0, 800)) {
  data %>%
    filter(metric == top) %>%
    mutate(ADJ = OBS_diff - .data[[adj_var]]) %>%
    ggplot(aes(x = ADJ)) +
    geom_histogram(aes(y = after_stat(count / sum(count))),
                   colour = "white", fill = "grey50", bins = 30) +
    geom_point(aes(x = mean(ADJ), y = -0.013), shape = 17, size = rel(1)) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    expand_limits(y = c(0, 0.3), x = xlims) +
    scale_y_continuous(breaks = c(0, 0.1, 0.2, 0.3))
}


experience_scatter <- function(rating_data, top = Inf) {
  plot_data <- rating_data %>%
    arrange(fed, sex, rating) %>%
    slice_tail(n = top, by = c(fed, sex)) %>%
    select(fed, sex, games) %>%
    summarise(games = mean(games), .by = c(fed, sex)) %>%
    pivot_wider(names_from = sex, values_from = games)
  #lims <- get_lims(plot_data = plot_data, factor = 20)
  lims <- c(0, 800)
  plot_data %>%
    ggplot(aes(y = F, x = M)) +
    geom_abline(aes(slope = 1, intercept = 0),
                linetype = "dashed", color = "grey40") +
    scale_y_continuous(breaks = c(0, 200, 400, 600, 800)) +
    scale_x_continuous(breaks = c(0, 200, 400, 600, 800)) +
    geom_point(size = 0.8) +
    expand_limits(x = lims, y = lims)
}


age_scatter <- function(data, top) {
  plot_data <- data %>%
    filter(metric == top) %>%
    select(fed, sex, age_obs) %>%
    pivot_wider(names_from = sex, values_from = age_obs)
  #lims <- get_lims(plot_data = plot_data, factor = 10)
  lims <- c(15, 70)
  plot_data %>%
    ggplot(aes(y = F, x = M)) +
    geom_abline(aes(slope = 1, intercept = 0),
                linetype = "dashed", color = "grey40") +
    geom_point(size = 0.8) +
    scale_y_continuous(breaks = seq(10, 70, 10)) +
    scale_x_continuous(breaks = seq(10, 70, 10)) +
    expand_limits(x = lims, y = lims)
}


figure_3 <- function(result_data, rating_data, feds_to_keep,
                     age_experience_data) {
  data <- reformat_results_data(result_data, rating_data, feds_to_keep) %>%
    inner_join(age_experience_data, by = join_by(fed, metric))

  rating_data <- filter(rating_data, fed %in% feds_to_keep)

  # First row: adjusted mean rating
  p11 <- adj_histogram_plot(data, top = "ALL", xlims = c(0, 800)) +
    labs(x = "P-adjusted gap (All)", y = "Proportion\nof federations")
  p12 <- adj_histogram_plot(data, top = "MAX10", xlims = c(0, 800)) +
    labs(x = "P-adjusted gap (Top 10)", y = "Proportion\nof federations")
  p13 <- adj_histogram_plot(data, top = "MAX1", xlims = c(0, 800)) +
    labs(x = "P-adjusted gap (Top 1)", y = "Proportion\nof federations")

  # Second row: experience women vs. age men
  p21 <- experience_scatter(rating_data, top = Inf) +
    labs(x = "Mean # Games M (All)", y = "Mean # Games W (All)")
  p22 <- experience_scatter(rating_data, top = 10) +
    labs(x = "Mean # Games M (Top 10)", y = "Mean # Games W (Top 10)")
  p23 <- experience_scatter(rating_data, top = 1) +
    labs(x = "# Games M (Top 1)", y = "# Games (Top 1)")

  # Third row: age women vs. age men
  p31 <- age_scatter(data, top = "ALL") +
    labs(x = "Mean Age M (All)", y = "Mean Age W (All)")
  p32 <- age_scatter(data, top = "MAX10") +
    labs(x = "Mean Age M (Top 10)", y = "Mean Age W (Top 10)")
  p33 <- age_scatter(data, top = "MAX1") +
    labs(x = "Age M (Top 1)", y = "Age W (Top 1)")

  p41 <- adj_histogram_plot(data, top = "ALL", adj_var = "yPEA", xlims = c(0, 800)) +
    labs(x = "PEA-adjusted gap (All)", y = "Proportion\nof federations")
  p42 <- adj_histogram_plot(data, top = "MAX10", adj_var = "yPEA", xlims = c(0, 800)) +
    labs(x = "PEA-adjusted gap (Top 10)", y = "Proportion\nof federations")
  p43 <- adj_histogram_plot(data, top = "MAX1", adj_var = "yPEA", xlims = c(0, 800)) +
    labs(x = "PEA-adjusted Top 1 gap", y = "Proportion\nof federations")

  # layout <- "
  # ABC
  # DEF
  # GHI
  # JKL
  # "

  layout <- "
  ADGJ
  BEHK
  CFIL
  "

  (p11 + p12 + p13 + p21 + p22 + p23 + p31 + p32 + p33 + p41 + p42 + p43) +
    plot_layout(guides = "collect", design = layout) +
    plot_annotation(tag_levels = "A") +
    theme(legend.position = 'bottom')  +
    theme(legend.title = ggtext::element_markdown())
}


figure_3(result_data = nulls(null_data, "juniors", "no_inactives"),
         rating_data = ratings(rating_data, "juniors", "no_inactives"),
         feds_to_keep = feds,
         age_experience_data = read_csv("data/age-experience-tab.csv",
                                        show_col_types = FALSE) %>%
           filter(floor == 1000, juniors, !inactives) %>%
           select(fed, yPEA, metric) %>%
           mutate(metric = case_match(metric, "mean" ~ "ALL", "top10" ~ "MAX10",
                                      "top1" ~ "MAX1"))) &
  theme(axis.title = element_text(size = rel(0.9)))
ggsave(file = "figures/fig_3_w_jun_no_ina.pdf", width = 10, height = 7)


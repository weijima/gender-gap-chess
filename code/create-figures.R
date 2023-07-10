library(magrittr)
library(data.table)
library(ggplot2)
library(foreach)
library(dplyr)
library(tidyr)
library(patchwork)
library(kableExtra)
library(cowplot)
library(ggtext)

theme_set(
  theme_minimal() +
    theme(axis.line = element_line(colour = "grey80"),
          axis.ticks = element_line(colour = "grey80"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank())
)

# data decisions main analysis:
# use fide data list from Dec 31 2019
# data downloaded from fide website on October 1 2021 https://ratings.fide.com/download_lists.phtml
# include all junior players for main analysis --> no filter on birthday
# exclude every player who has an "i" or an "wi" flag
# Age is 2019 minus birth year.
chess <- fread("data/rating-data.csv")  |>
  mutate(born = ifelse(born == 0, NA, born)) |>
  mutate(age = year(as.Date("2019-01-01")) - born)

raw_data_w_juniors_no_inactive_worldwide <- chess |>
  filter(sex != "",
         active)

raw_data_w_juniors_w_inactive_worldwide <- chess |>
  filter(sex != "",
         age > 18)
raw_data_no_juniors_no_inactive_worldwide <- chess %>%
  filter(sex != "",
         age > 18,
         active)
raw_data_no_juniors_w_inactive_worldwide <- chess %>%
  filter(sex != "",
         age > 18)

# results from Richard
data_w_juniors_no_inactive <- fread("data/nulls-Richard/nulls-J1-I0.csv")
data_w_juniors_w_inactive <- fread("data/nulls-Richard/nulls-J1-I1.csv")
data_no_juniors_no_inactive <- fread("data/nulls-Richard/nulls-J0-I0.csv")
data_no_juniors_w_inactive <- fread("data/nulls-Richard/nulls-J0-I1.csv")

# federations to include
feds <- data_w_juniors_no_inactive |>
  filter(FED != "ALL") |>
  pull(FED) |>
  unique()
length(feds)




# Figure 1
create_histogram <- function(data, bwidth = 100) {
  hist_F <- data %>%
    dplyr::filter(sex == "F") %>%
    dplyr::mutate(sex = "Women") %>%
    ggplot(aes(x = rating)) +
    geom_histogram(aes(y = stat(count) / sum(count)),
                   colour = "white", binwidth = bwidth) +
    facet_wrap(~ sex) +
    scale_y_continuous(limits = c(0, 0.15)) +
    scale_x_continuous(limits = c(1000, 2900)) +
    labs(y = "Proportion", x = "Rating")

  hist_M <- data %>%
    dplyr::filter(sex == "M") %>%
    dplyr::mutate(sex = "Men") %>%
    ggplot(aes(x = rating)) +
    geom_histogram(aes(y = stat(count) / sum(count)),
                   colour = "white", binwidth = bwidth) +
    facet_wrap(~ sex) +
    # # theme_minimal() +
    scale_y_continuous(limits = c(0, 0.15)) +
    scale_x_continuous(limits = c(1000, 2900)) +
    labs(y = "Proportion", x = "Rating")

  hist_all <- hist_F + hist_M +
    theme(axis.title.y = element_blank())

  return(hist_all)
}

hist <- create_histogram(raw_data_w_juniors_no_inactive_worldwide)
ggsave(plot = hist, file = "data/figures/fig_1_w_jun_no_ina.png",
       width = 3.7, height = 2)

hist <- create_histogram(raw_data_w_juniors_w_inactive_worldwide)
ggsave(plot = hist, file = "data/figures/fig_1_w_jun_w_ina.png",
       width = 3.7, height = 2)

hist <- create_histogram(raw_data_no_juniors_no_inactive_worldwide)
ggsave(plot = hist, file = "data/figures/fig_1_no_jun_no_ina.png",
       width = 3.7, height = 2)

hist <- create_histogram(raw_data_no_juniors_w_inactive_worldwide)
ggsave(plot = hist, file = "data/figures/fig_1_no_jun_w_ina.png",
       width = 3.7, height = 2)







# Figure 2
get_summary_stats <- function(rating_data, feds_to_keep) {
  rating_data <- rating_data |>
    filter(fed %in% feds_to_keep) |>
    select(country = fed, sex, rating, age) |>
    arrange(country, sex, -rating) |>
    group_by(country, sex)

  rel_F <- rating_data |>
    group_by(country) |>
    mutate(rel_F = sum(sex == "F")/n()) |>
    select(country, rel_F) |>
    unique()

  mean_all <- rating_data |>
    summarise(observed = mean(rating),
              age_obs = mean(age, na.rm = TRUE)) |>
    mutate(group = "ALL")

  sd_all <- rating_data |>
    summarise(observed = sd(rating),
              age_obs = mean(age, na.rm = TRUE)) |>
    mutate(group = "SD")

  mean_10 <- rating_data |>
    slice(1:10) |>
    summarise(observed = mean(rating),
              age_obs = mean(age, na.rm = TRUE)) |>
    # summarise(observed = mean(rating),
    #           sd_observed = sd(rating)) |>
    mutate(group = "MAX10")

  mean_1 <- rating_data |>
    slice(1) |>
    mutate(age_obs = mean(age, na.rm = TRUE)) |>
    rename(observed = rating) |>
    select(-age) |>
    mutate(group = "MAX1")

  rbind(mean_1, mean_10, mean_all, sd_all) |>
    ungroup() |>
    inner_join(rel_F) |>
    rename(FED = country)
}

reformat_results_data <- function(results_data, rating_data, feds_to_keep) {
  out <- results_data |>
    select(FED,
           MAX1_OBS, MAX1_PTMEAN, MAX1_PTPVAL,
           MAX10_OBS,MAX10_PTMEAN, MAX10_PTPVAL,
           ALL_OBS = MEAN_OBS, ALL_PTMEAN = MEAN_PTMEAN, ALL_PTPVAL = MEAN_PTPVAL,
           SD_OBS, SD_PTMEAN, SD_PTPVAL,
           MAX1_AGEDIFF = AGEDIFFMAX1,
           MAX10_AGEDIFF = AGEDIFFMAX10,
           ALL_AGEDIFF = AGEDIFFOVERALL) |>
    pivot_longer(cols = contains("_")) |>
    rowwise() |>
    mutate(group = strsplit(name, "[_]")[[1]][1],
           name = strsplit(name, "[_]")[[1]][2]) |>
    pivot_wider(names_from = name, values_from = value)

  summary <- get_summary_stats(rating_data, feds_to_keep)

  out |>
    inner_join(summary) |>
    rename(OBS_diff = OBS)
}


get_lims <- function(plot_data, factor = 100) {
  ylims <- c(floor(min(plot_data$F) / factor) * factor,
             ceiling(max(plot_data$F) / factor) * factor)
  xlims <- c(floor(min(plot_data$M) / factor) * factor,
             ceiling(max(plot_data$M) / factor) * factor)

  lims <- c(min(ylims[1], xlims[1]),
            max(ylims[2], xlims[2]))
  return(lims)
}

scatter_plot <- function(data, top = "ALL",
                         lims = NULL) {
  plot_data <- data |>
    filter(group == top) |>
    select(FED, sex, PTMEAN, PTPVAL, observed) |>
    pivot_wider(values_from = observed, names_from = sex) |>
    mutate(
      `*p*-value` = ifelse(PTPVAL < 0.05 | PTPVAL > 0.95, "significant", "not significant")

    )

  if (is.null(lims)) {
    lims <- get_lims(plot_data = plot_data)
  }

  plot_data |>
    ggplot(aes(y = F, x = M)) +
    geom_point(aes(colour = `*p*-value`), size = 0.8) +
    geom_abline(aes(slope = 1, intercept = 0),
                linetype = "dashed", color = "grey40") +
    coord_cartesian(xlim = lims, ylim = lims) +
    theme(legend.position = "none") +
    scale_color_manual(values = c("black", "tomato3"))
}

histogram_plot <- function(data, top, xlims = NULL) {
  data |>
    filter(group == top) %>%
    ggplot(aes(x = OBS_diff)) +
    geom_histogram(aes(y = stat(count) / sum(count)),
                   colour = "white", fill = "grey50") +
    geom_vline(xintercept = 0, linetype = "dashed") +
    expand_limits(y = c(0, 0.2), x = xlims) +
    scale_y_continuous(breaks = c(0, 0.05, 0.1, 0.15, 0.2))

  # expand_limits(y = c(0, 0.12), x = xlims) +
  # scale_y_continuous(breaks = c(0, 0.03, 0.06, 0.09, 0.12))
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
    labs(x = "\u0394 mean rating", y = "Proportion")
  p22 <- histogram_plot(data, top = "MAX10", xlims = c(0, 800)) +
    labs(x = "\u0394 mean rating (Top 10)", y = "Proportion")
  p23 <- histogram_plot(data, top = "MAX1", xlims = c(0, 800)) +
    labs(x = "\u0394 rating (Top 1)", y = "Proportion")
  p24 <- histogram_plot(data, top = "SD", xlims = c(0, 200)) +
    labs(x = "\u0394 SD rating", y = "Proportion")

  layout <- "
  ACEG
  BDFH
  "

  (p11 + p21 + p12 + p22 + p13 + p23 + p14 + p24) +
    plot_layout(guides = "collect",
                # heights = c(2, 1),
                design = layout) +
    plot_annotation(tag_levels = "A") +
    theme(legend.position = 'bottom')  +
    theme(legend.title = element_markdown())
}


fig2 <- reformat_results_data(data_w_juniors_no_inactive,
                              raw_data_w_juniors_no_inactive_worldwide,
                              feds) |>
  figure_2()
ggsave(plot = fig2, file = "data/figures/fig_2_w_jun_no_ina.png",
       width = 8, height = 4)

fig2 <- reformat_results_data(data_w_juniors_w_inactive,
                              raw_data_w_juniors_w_inactive_worldwide,
                              feds) |>
  figure_2()
ggsave(plot = fig2, file = "data/figures/fig_2_w_jun_w_ina.png",
       width = 8, height = 4)

fig2 <- reformat_results_data(data_no_juniors_no_inactive,
                              raw_data_no_juniors_no_inactive_worldwide,
                              feds) |>
  figure_2()
ggsave(plot = fig2, file = "data/figures/fig_2_no_jun_no_ina.png",
       width = 8, height = 4)

fig2 <- reformat_results_data(data_no_juniors_w_inactive,
                              raw_data_no_juniors_w_inactive_worldwide,
                              feds) |>
  figure_2()
ggsave(plot = fig2, file = "data/figures/fig_2_no_jun_w_ina.png",
       width = 8, height = 4)










# Figure 3
adj_histogram_plot <- function(data, top, adj_var = "PTMEAN", xlims = NULL) {
  data |>
    filter(group == top) |>
    mutate(ADJ = OBS_diff - .data[[adj_var]]) |>
    ggplot(aes(x = ADJ)) +
    geom_histogram(aes(y = stat(count) / sum(count)),
                   colour = "white", fill = "grey50") +
    geom_vline(xintercept = 0, linetype = "dashed") +
    expand_limits(y = c(0, 0.2), x = xlims) +
    scale_y_continuous(breaks = c(0, 0.05, 0.1, 0.15, 0.2))
  # expand_limits(y = c(0, 0.12), x = xlims) +
  # scale_y_continuous(breaks = c(0, 0.03, 0.06, 0.09, 0.12))
}

experience_scatter <- function(rating_data, top = Inf) {
  plot_data <- rating_data |>
    arrange(fed, sex, rating) |>
    group_by(fed, sex) |>
    slice_tail(n = top) |>
    select(fed, sex, games) |>
    group_by(fed, sex) |>
    summarise(games = mean(games)) |>
    pivot_wider(names_from = sex, values_from = games)

  lims <- get_lims(plot_data = plot_data, factor = 20)
  lims <- c(0, 800)

  plot_data |>
    ggplot(aes(y = F, x = M)) +
    geom_abline(aes(slope = 1, intercept = 0),
                linetype = "dashed", color = "grey40") +
    geom_point(size = 0.8) +
    expand_limits(x = lims, y = lims)
}


age_scatter <- function(data, top) {
  plot_data <- data |>
    filter(group == top) |>
    select(FED, sex, age_obs) |>
    pivot_wider(names_from = sex, values_from = age_obs)

  lims <- get_lims(plot_data = plot_data, factor = 10)
  lims <- c(15, 70)

  plot_data |>
    ggplot(aes(y = F, x = M)) +
    geom_abline(aes(slope = 1, intercept = 0),
                linetype = "dashed", color = "grey40") +
    geom_point(size = 0.8) +
    expand_limits(x = lims, y = lims)
}

figure_3 <- function(result_data, rating_data, feds_to_keep,
                     age_experience_data) {

  data <- reformat_results_data(result_data, rating_data, feds_to_keep) |>
    inner_join(age_experience_data)

  rating_data <- filter(rating_data, fed %in% feds_to_keep)

  # first row: adjusted mean rating
  p11 <- adj_histogram_plot(data, top = "ALL", xlims = c(0, 800)) +
    labs(x = "Overall \u0394 mean rating", y = "Proportion")
  p12 <- adj_histogram_plot(data, top = "MAX10", xlims = c(0, 800)) +
    labs(x = "P-adj. \u0394 mean rating (Top 10)", y = "Proportion")
  p13 <- adj_histogram_plot(data, top = "MAX1", xlims = c(0, 800)) +
    labs(x = "P-adj. \u0394 rating (Top 1)", y = "Proportion")

  # second row: experiene women vs. age men.
  p21 <- experience_scatter(rating_data, top = Inf) +
    labs(x = "Mean # Games M", y = "Mean # Games W")
  p22 <- experience_scatter(rating_data, top = 10) +
    labs(x = "Mean # Games M (Top 10)", y = "# Games W (Top 10)")
  p23 <- experience_scatter(rating_data, top = 1) +
    labs(x = "# Games M (Top 1)", y = "# Games (Top 1)")

  # Third row: Age women vs. age men
  p31 <- age_scatter(data, top = "ALL") +
    labs(x = "Mean Age M", y = "Mean Age W")
  p32 <- age_scatter(data, top = "MAX10") +
    labs(x = "Mean Age M (Top 10)", y = "Mean Age W (Top 10)")
  p33 <- age_scatter(data, top = "MAX1") +
    labs(x = "Age M (Top 1)", y = "Age W (Top 1)")

  p41 <- adj_histogram_plot(data, top = "ALL", adj_var = "yPEA", xlims = c(0, 800)) +
    labs(x = "PEA-adjusted mean gap", y = "Proportion")
  p42 <- adj_histogram_plot(data, top = "MAX10", adj_var = "yPEA", xlims = c(0, 800)) +
    labs(x = "PEA-adjusted top 10 gap", y = "Proportion")
  p43 <- adj_histogram_plot(data, top = "MAX1", adj_var = "yPEA", xlims = c(0, 800)) +
    labs(x = "PEA-adjusted top 1 gap", y = "Proportion")

  layout <- "
  ABC
  DEF
  GHI
  JKL
  "

  (p11 + p12 + p13 + p21 + p22 + p23 + p31 + p32 + p33 + p41 + p42 + p43) +
    plot_layout(guides = "collect",
                design = layout) +
    plot_annotation(tag_levels = "A") +
    theme(legend.position = 'bottom')  +
    theme(legend.title = element_markdown())
}

age_experience_data <- fread("data/age-experience-tab.csv")
age_exp_w_juniors_no_ina <- age_experience_data |>
  filter(floor == 1000,
         juniors) |>
  select(FED = fed, yPEA, group = metric) |>
  mutate(group = case_match(
    group,
    "mean" ~ "ALL",
    "top10" ~ "MAX10",
    "top1" ~ "MAX1"
  ))

fig3 <- figure_3(result_data = data_w_juniors_no_inactive,
                 rating_data = raw_data_w_juniors_no_inactive_worldwide,
                 feds_to_keep = feds,
                 age_experience_data = age_exp_w_juniors_no_ina)
ggsave(plot = fig3, file = "data/figures/fig_3_w_jun_no_ina.png",
       width = 8, height = 6)



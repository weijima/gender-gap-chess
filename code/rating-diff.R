library(tidyverse)


p_anal <- function(pvalues, signif = 0.05, method = "fdr") {
  p_female <- p.adjust(1 - pvalues, method = method)
  p_male <- p.adjust(pvalues, method = method)
  # The factor of 2 simply introduces a symbol to distinguish women (2) from men (1):
  signif_female <- 2L * (p_female < signif / 2)
  signif_male <- 1L * (p_male < signif / 2)
  # The nonzero entries of signif_female and signif_male are completely nonoverlapping:
  s <- signif_female + signif_male
  # Translate the arbitrary symbols 2 and 1 into test describing significance:
  case_when(
    s %in% 1:2 ~ "Significant",
    s == 0 ~ "Non-significant",
    .default = "ERROR - BOTH SEXES ARE SIGNIFICANT"
  )
}


# Table of raw p-values, along with the corrected significance for each federation:
p_values <- function(null_data = "data/null-stats.csv") {
  read_csv(null_data, show_col_types = FALSE) %>%
    filter(fed != "ALL", stat == "ptpval") %>%
    select(-stat) %>%
    mutate(fdr = p_anal(value),
           none = p_anal(value, method = "none"),
           .by = c(juniors, inactives, floor, metric))
}


read_csv("data/null-stats.csv", show_col_types = FALSE) %>%
  filter(fed != "ALL", stat == "obs") %>%
  rename(y = value) %>%
  left_join(read_csv("data/age-experience-tab.csv", show_col_types = FALSE),
            by = join_by(metric, juniors, inactives, floor, fed)) %>%
  select(-c(stat, E, A, weight)) %>%
  full_join(p_values("data/null-stats.csv"),
            by = join_by(juniors, inactives, floor, metric, fed)) %>%
  pivot_longer(cols = c(fdr, none), names_to = "method", values_to = "signif") %>%
  filter(method == "fdr") %>%
  filter(metric %in% c("mean", "top1", "top10")) %>%
  mutate(metric = case_when(
    metric == "mean" ~ "Mean gap (All)",
    metric == "top10" ~ "Mean gap (Top 10)",
    metric == "top1" ~ "Gap (Top 1)"
  )) %>%
  mutate(metric = fct_relevel(metric, "Mean gap (All)", "Mean gap (Top 10)")) %>%
  mutate(juniors = ifelse(juniors, "With juniors", "No juniors")) %>%
  mutate(inactives = ifelse(inactives, "with inactives", "no inactives")) %>%
  mutate(filter = fct_rev(str_c(juniors, ", ", inactives)), .before = 1) %>%
  pivot_longer(cols = starts_with("y"), names_to = "response", values_to = "gap") %>%
  mutate(signif = ifelse(response == "yP", signif, strrep(" ", 55))) %>%
  mutate(signif = fct_relevel(signif, strrep(" ", 55), "Significant")) %>%
  mutate(floor = as_factor(floor)) %>%
  mutate(response = case_match(
    response,
    "y" ~ "Unadjusted\n",
    "yP" ~ "Participation-adjusted\n",
    "yPEA" ~ "PEA-adjusted\n"
  )) %>%
  mutate(response = fct_relevel(response, "Unadjusted\n","Participation-adjusted\n")) %>%
  ggplot() +
  geom_boxplot(aes(x = floor, y = gap, fill = filter), outlier.shape = NA) +
  geom_point(aes(x = floor, y = gap, colour = filter, alpha = signif, shape = signif),
             position = position_jitterdodge(jitter.width = 0.06, seed = 54321)) +
  geom_hline(yintercept = 0, alpha = 0.5, linetype = "dashed") +
  labs(x = "Rating floor", y = "Mean rating gap (men - women)") +
  facet_grid(metric ~ response, scale = "free_y", switch = "y") +
  scale_shape_manual(name = "", values = c(1, 19, 19), guide = "none") +
  scale_colour_manual(name = "",
                      values = rev(c("steelblue","forestgreen","goldenrod","plum3"))) +
  scale_fill_manual(name = "", values = rep("white", 4)) +
  scale_alpha_manual(name = "", values = c(0.7, 1, 0.2)) +
  guides(alpha = guide_legend(nrow = 1, order = 1,
                              override.aes = list(shape = c(NA, 19, 19))),
         colour = guide_legend(nrow = 1, order = 2, override.aes = list(alpha = 1)),
         fill = guide_legend(nrow = 1, order = 2)) +
  theme_minimal(base_size = 14) +
  theme(axis.line = element_line(colour = "grey80"),
        axis.ticks = element_line(colour = "grey80"),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        strip.placement = "outside",
        legend.position = c(0.5, 0.735),
        legend.box = "vertical",
        legend.direction = "vertical",
        legend.spacing.y = unit(18.6, "lines"),
        legend.margin = margin(0, 0, 0, 0),
        plot.margin = unit(c(0.1, 0.1, 1.3, 0.1), "cm"))
#ggsave("figures/fig_4.pdf", width = 10, height = 8.57)


read_csv("data/null-stats.csv", show_col_types = FALSE) %>%
  filter(fed != "ALL", stat == "obs") %>%
  rename(y = value) %>%
  left_join(read_csv("data/age-experience-tab.csv", show_col_types = FALSE),
            by = join_by(metric, juniors, inactives, floor, fed)) %>%
  select(-c(stat, E, A, weight)) %>%
  filter(metric %in% c("mean", "top1", "top10")) %>%
  mutate(metric = case_when(
    metric == "mean" ~ "Mean gap (All)",
    metric == "top10" ~ "Mean gap (Top 10)",
    metric == "top1" ~ "Gap (Top 1)"
  )) %>%
  mutate(metric = fct_relevel(metric, "Mean gap (All)", "Mean gap (Top 10)")) %>%
  mutate(juniors = ifelse(juniors, "With juniors", "No juniors")) %>%
  mutate(inactives = ifelse(inactives, "with inactives", "no inactives")) %>%
  mutate(filter = fct_rev(str_c(juniors, ", ", inactives)), .before = 1) %>%
  summarise(across(starts_with("y"), function(x) round(mean(x), 2)),
            .by = c(filter, floor, metric)) %>%
  mutate(floor = as_factor(floor)) %>%
  pivot_longer(cols = starts_with("y"), names_to = "response", values_to = "gap") %>%
  mutate(response = case_match(
    response,
    "y" ~ "Unadjusted",
    "yP" ~ "Participation-adjusted",
    "yPEA" ~ "PEA-adjusted"
  )) %>%
  mutate(response = fct_relevel(response, "Unadjusted", "Participation-adjusted")) %>%
  arrange(metric, response, floor, filter) %>%
  mutate(gap = gap / first(gap), .by = c(metric)) %>%
  mutate(filter = fct_rev(filter)) %>%
  ggplot() +
  geom_label(aes(x = floor, y = filter, fill = gap,
                 label = scales::percent(gap, accuracy = 1))) +
  labs(x = "Rating floor", y = "Mean of mean rating gap (men - women)") +
  facet_grid(metric ~ response, switch = "y") +
  scale_fill_gradient(low = "white", high = "grey60", labels = scales::percent,
                      breaks = c(0.5, 1), name = "fraction of original gap ") +
  theme_minimal(base_size = 14) +
  theme(axis.line = element_line(colour = "grey80"),
        axis.ticks = element_line(colour = "grey80"),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.position = "bottom",
        strip.placement = "outside")


read_csv("data/null-stats.csv", show_col_types = FALSE) %>%
  filter(fed == "ALL", stat %in% c("obs", "ptmean")) %>%
  pivot_wider(names_from = stat, values_from = value) %>%
  rename(`Unadjusted` = obs, `Participation-adjusted` = ptmean) %>%
  filter(metric %in% c("mean", "top1", "top10")) %>%
  mutate(metric = case_when(
    metric == "mean" ~ "Mean gap (All)",
    metric == "top10" ~ "Mean gap (Top 10)",
    metric == "top1" ~ "Gap (Top 1)"
  )) %>%
  mutate(metric = fct_relevel(metric, "Mean gap (All)", "Mean gap (Top 10)")) %>%
  mutate(juniors = ifelse(juniors, "With juniors", "W/o juniors")) %>%
  mutate(inactives = ifelse(inactives, "with inactives", "w/o inactives")) %>%
  mutate(filter = fct_rev(str_c(juniors, ", ", inactives)), .before = 1) %>%
  summarise(across(contains("adjusted"), function(x) round(mean(x), 2)),
            .by = c(filter, floor, metric)) %>%
  mutate(floor = as_factor(floor)) %>%
  mutate(`Participation-adjusted` = Unadjusted - `Participation-adjusted`) %>%
  pivot_longer(cols = contains("adjusted"), names_to = "response", values_to = "gap") %>%
  mutate(response = fct_relevel(response, "Unadjusted", "Participation-adjusted")) %>%
  arrange(metric, response, floor, filter) %>%
  mutate(gap = gap / gap[1], .by = c(metric)) %>%
  mutate(filter = fct_rev(filter)) %>%
  ggplot() +
  geom_label(aes(x = floor, y = filter, fill = gap,
                 label = scales::percent(gap, accuracy = 1))) +
  labs(x = "Rating floor", y = "Mean of mean rating gap (men - women)") +
  facet_grid(metric ~ response, switch = "y") +
  scale_fill_gradient(low = "white", high = "grey60", labels = scales::percent,
                      breaks = c(0.5, 1), name = "fraction of original gap ") +
  theme_minimal(base_size = 14) +
  theme(axis.line = element_line(colour = "grey80"),
        axis.ticks = element_line(colour = "grey80"),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.position = "bottom",
        strip.placement = "outside")

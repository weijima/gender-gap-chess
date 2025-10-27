library(tidyverse)


human_readable_labels <- function(data) {
  data %>%
    mutate(metric = case_when(
      metric == "mean" ~ "Overall mean gap",
      metric == "top10" ~ "Top 10 gap",
      metric == "top1" ~ "Top 1 gap "
    )) %>%
    mutate(metric = fct_relevel(metric, "Overall mean gap", "Top 10 gap")) %>%
    mutate(juniors = ifelse(juniors, "With juniors", "W/o juniors")) %>%
    mutate(inactives = ifelse(inactives, "with inactives", "w/o inactives")) %>%
    mutate(filter = str_c(juniors, ", ", inactives), .before = 1) %>%
    mutate(filter = fct_relevel(
      filter,
      "With juniors, w/o inactives",
      "With juniors, with inactives",
      "W/o juniors, with inactives",
      "W/o juniors, w/o inactives"
    ))
}


summary_plot_data <- read_csv("data/null-stats.csv", col_types = "llicccd") %>%
  filter(fed != "ALL", stat %in% c("obs", "ptpval")) %>%
  pivot_wider(names_from = stat, values_from = value) %>%
  rename(y = obs, raw_pval = ptpval) %>%
  mutate(pval = 2 * pmin(raw_pval, 1 - raw_pval)) %>%
  mutate(adj_pval = p.adjust(pval, method = "fdr"),
         .by = c(juniors, inactives, floor, metric)) %>%
  left_join(read_csv("data/age-experience-tab.csv", col_types = "cllicddddd"),
            by = join_by(metric, juniors, inactives, floor, fed)) %>%
  select(!E & !A & !weight) %>%
  relocate(y, .before = yP) %>%
  mutate(fdr = ifelse(adj_pval < 0.05, "Significant", "Non-significant"),
         none = ifelse(pval < 0.05, "Significant", "Non-significant")) %>%
  pivot_longer(cols = c(fdr, none), names_to = "method", values_to = "signif") %>%
  filter(method == "fdr") %>%
  filter(metric %in% c("mean", "top1", "top10")) %>%
  human_readable_labels() %>%
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
  mutate(response = fct_relevel(response, "Unadjusted\n","Participation-adjusted\n"))


label_data <- summary_plot_data %>%
  distinct(metric, response) %>%
  arrange(metric, response) %>%
  mutate(label = LETTERS[1:9]) %>%
  mutate(hjust = c(3.8, 3.8, 3.6, 3.6, 3.8, 4.0, 3.3, 3.6, 8.5))


summary_plot_data %>%
  ggplot() +
  geom_boxplot(aes(x = floor, y = gap, fill = filter), outlier.shape = NA) +
  geom_point(aes(x = floor, y = gap, colour = filter, alpha = signif, shape = signif),
             position = position_dodge(width = 0.75)) +
  geom_hline(yintercept = 0, alpha = 0.5, linetype = "dashed") +
  geom_text(data = label_data, x = "1000", y = Inf, size = 5, vjust = 1.5,
            aes(label = label, hjust = hjust)) +
  labs(x = "Rating floor", y = "Rating gap") +
  facet_grid(metric ~ response, scale = "free_y", switch = "y") +
  scale_shape_manual(name = "", values = c(1, 19, 19), guide = "none") +
  scale_colour_viridis_d(name = "", option = "C", end = 0.85, direction = -1) +
  scale_fill_manual(name = "", values = rep("white", 4)) +
  scale_alpha_manual(name = "", values = c(0.7, 1, 0.2)) +
  guides(alpha = guide_legend(nrow = 1, order = 1,
                              override.aes = list(shape = c(NA, 19, 19))),
         colour = guide_legend(nrow = 1, order = 2, override.aes = list(alpha = 1)),
         fill = guide_legend(nrow = 1, order = 2)) +
  theme_minimal(base_size = 14) +
  theme(panel.grid = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_line(colour = "grey80"),
        panel.border = element_rect(colour = "grey80", fill = NA),
        panel.background = element_blank(),
        strip.background = element_blank(),
        strip.placement = "outside",
        legend.position = "inside",
        legend.position.inside = c(0.5, 0.48),
        legend.box = "vertical",
        legend.direction = "vertical",
        legend.spacing.y = unit(36.75, "lines"),
        legend.margin = margin(0, 0, 0, 0),
        legend.background = element_blank(),
        plot.margin = unit(c(0.1, 0.1, 1.3, 0.1), "cm"))
#ggsave("figures/summary-fig.pdf", device = cairo_pdf, width = 10, height = 8.57)


read_csv("data/null-stats.csv", col_types = "llicccd") %>%
  filter(fed != "ALL", stat == "obs") %>%
  rename(y = value) %>%
  left_join(read_csv("data/age-experience-tab.csv", col_types = "cllicddddd"),
            by = join_by(metric, juniors, inactives, floor, fed)) %>%
  select(!c(stat, E, A, weight)) %>%
  filter(metric %in% c("mean", "top1", "top10")) %>%
  human_readable_labels() %>%
  mutate(filter = fct_rev(filter)) %>%
  summarise(across(starts_with("y"), mean), .by = c(filter, floor, metric)) %>%
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
  mutate(gap = gap / gap[4], .by = c(metric)) %>%
  ggplot() +
  geom_label(aes(x = floor, y = filter, fill = gap,
                 label = scales::percent(gap, accuracy = 1))) +
  labs(
    x = "Rating floor",
    y = expression(paste("Rating gap ", (M - W), ", averaged over federatons"))
  ) +
  facet_grid(metric ~ response, switch = "y") +
  scale_fill_gradient(low = "white", high = "grey60", labels = scales::percent,
                      breaks = c(0.5, 1), name = "Fraction of original gap ") +
  theme_minimal(base_size = 14) +
  theme(axis.line = element_line(colour = "grey80"),
        axis.ticks = element_line(colour = "grey80"),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.position = "bottom",
        strip.placement = "outside")
#ggsave("figures/per-fed-rating-percentage.pdf", width = 8, height = 6)


read_csv("data/null-stats.csv", col_types = "llicccd") %>%
  filter(fed == "ALL", stat %in% c("obs", "ptmean")) %>%
  pivot_wider(names_from = stat, values_from = value) %>%
  filter(metric %in% c("mean", "top1", "top10")) %>%
  mutate(ptmean = obs - ptmean) %>%
  pivot_longer(cols = c(obs, ptmean), names_to = "stat", values_to = "gap") %>%
  mutate(stat = case_match(stat, "obs" ~ "Unadjusted",
                           "ptmean" ~ "Participation-adjusted")) %>%
  human_readable_labels() %>%
  mutate(filter = fct_rev(filter)) %>%
  mutate(across(c(floor, stat), as_factor)) %>%
  arrange(metric, stat, floor, filter) %>%
  select(metric, stat, floor, filter, gap) %>%
  mutate(gap = gap / gap[4], .by = c(metric)) %>%
  mutate(gap = ifelse(floor!=1400 & stat=="Unadjusted" & metric!="Overall mean gap",
                      NA, gap)) %>%
  mutate(gap_label = str_c(round(100 * gap), "%")) %>%
  mutate(
    gap_label = ifelse(floor==1400 & stat=="Unadjusted" & metric != "Overall mean gap",
                       str_c(strrep(" ", 17), gap_label,strrep(" ", 17)),
                       gap_label)
  ) %>%
  ggplot() +
  geom_label(aes(x = floor, y = filter, fill = gap, label = gap_label)) +
  labs(
    x = "Rating floor",
    y = expression(paste("Rating gap ", (M - W)))
  ) +
  facet_grid(metric ~ stat, switch = "y") +
  scale_fill_gradient(low = "white", high = "grey60", labels = scales::percent,
                      breaks = c(0.5, 1), name = "Fraction of original gap ") +
  theme_minimal(base_size = 14) +
  theme(axis.line = element_line(colour = "grey80"),
        axis.ticks = element_line(colour = "grey80"),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.position = "bottom",
        strip.placement = "outside")
#ggsave("figures/global-rating-percentage.pdf", width = 8, height = 6)

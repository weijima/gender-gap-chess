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
    metric == "mean" ~ "Mean gap",
    metric == "top10" ~ "Top 10 gap",
    metric == "top1" ~ "Top gap "
  )) %>%
  mutate(metric = fct_relevel(metric, "Mean gap", "Top 10 gap")) %>%
  mutate(juniors = ifelse(juniors, "With juniors", "W/o juniors")) %>%
  mutate(inactives = ifelse(inactives, "with inactives", "w/o inactives")) %>%
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
  labs(x = "Rating floor",
       y = expression(paste("Rating gap (men ", phantom() - phantom()," women)"))) +
  facet_grid(metric ~ response, scale = "free_y", switch = "y") +
  scale_shape_manual(name = "", values = c(1, 19, 19), guide = "none") +
  scale_colour_viridis_d(name = "", option = "C", end = 0.85, direction = -1) +
  scale_fill_manual(name = "", values = rep("white", 4)) +
  scale_alpha_manual(name = "", values = c(0.7, 1, 0.2)) +
  guides(alpha = guide_legend(nrow = 1, order = 1,
                              override.aes = list(shape = c(NA, 19, 19))),
         colour = guide_legend(nrow = 1, order = 2, override.aes = list(alpha = 1)),
         fill = guide_legend(nrow = 1, order = 2)) +
  theme_bw(base_size = 14) +
  #theme_minimal(base_size = 14) +
  theme(panel.grid = element_blank(),
        #axis.line = element_line(colour = "grey80"),
        #axis.ticks = element_line(colour = "grey80"),
        #panel.border = element_blank(),
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
#ggsave("figures/summary-fig.pdf", width = 10, height = 8.57)


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
  mutate(juniors = ifelse(juniors, "With juniors", "W/o juniors")) %>%
  mutate(inactives = ifelse(inactives, "with inactives", "w/o inactives")) %>%
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
  filter(metric %in% c("mean", "top1", "top10")) %>%
  mutate(ptmean = obs - ptmean) %>%
  pivot_longer(cols = c(obs, ptmean), names_to = "stat", values_to = "gap") %>%
  mutate(stat = case_match(stat, "obs" ~ "Unadjusted",
                           "ptmean" ~ "Participation-adjusted")) %>%
  mutate(metric = case_match(metric, "mean" ~ "Mean gap (All)",
                             "top10" ~ "Mean gap (Top 10)", "top1" ~ "Gap (Top 1)")) %>%
  mutate(juniors = ifelse(juniors, "With juniors", "W/o juniors")) %>%
  mutate(inactives = ifelse(inactives, "with inactives", "w/o inactives")) %>%
  mutate(filter = str_c(juniors, ", ", inactives), .before = 1) %>%
  mutate(across(c(filter, floor, stat), as_factor),
         metric = fct_relevel(metric, "Mean gap (All)", "Mean gap (Top 10)")) %>%
  arrange(metric, stat, floor, filter) %>%
  select(metric, stat, floor, filter, gap) %>%
  mutate(gap = gap / gap[4], .by = c(metric)) %>%
  mutate(gap = ifelse(floor!=1400 & stat=="Unadjusted" & metric!="Mean gap (All)",
                      NA, gap)) %>%
  mutate(gap_label = str_c(round(100 * gap), "%")) %>%
  mutate(gap_label = ifelse(floor==1400 & stat=="Unadjusted" & metric!="Mean gap (All)",
                            str_c(strrep(" ",17),gap_label,strrep(" ",17)),gap_label))%>%
  ggplot() +
  geom_label(aes(x = floor, y = filter, fill = gap, label = gap_label)) +
  labs(x = "Rating floor", y = "Mean of mean rating gap (men - women)") +
  facet_grid(metric ~ stat, switch = "y") +
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
#ggsave("figures/global-rating-percentage.pdf", width = 7, height = 6)

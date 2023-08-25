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
    .default = "ERROR - BOTH MALES AND FEMALES ARE SIGNIFICANT"
  )
}


# Table of raw p-values, along with the corrected significance for each federation:
pvalues <- read_csv("data/null-stats.csv", show_col_types = FALSE) %>%
  filter(fed != "ALL", stat == "ptpval") %>%
  select(-stat) %>%
  mutate(fdr = p_anal(value),
         none = p_anal(value, method = "none"),
         .by = c(juniors, inactives, floor, metric))


read_csv("data/null-stats.csv", show_col_types = FALSE) %>%
  filter(fed != "ALL", stat == "obs") %>%
  rename(y = value) %>%
  left_join(read_csv("data/age-experience-tab.csv", show_col_types = FALSE),
            by = join_by(metric, juniors, inactives, floor, fed)) %>%
  select(-c(stat, E, A, weight)) %>%
  full_join(pvalues, by = join_by(juniors, inactives, floor, metric, fed)) %>%
  pivot_longer(cols = c(fdr, none), names_to = "method", values_to = "signif") %>%
  filter(method == "fdr") %>%
  filter(metric %in% c("mean", "top1", "top10")) %>%
  mutate(metric = case_when(
    metric == "mean" ~ "All",
    metric == "top10" ~ "Top 10",
    metric == "top1" ~ "Top 1"
  )) %>%
  mutate(metric = fct_relevel(metric, "Top 1", "Top 10")) %>%
  mutate(juniors = ifelse(juniors, "With juniors", "No juniors")) %>%
  mutate(inactives = ifelse(inactives, "with inactives", "no inactives")) %>%
  mutate(filter = fct_rev(str_c(juniors, ", ", inactives)), .before = 1) %>%
  pivot_longer(cols = starts_with("y"), names_to = "response", values_to = "gap") %>%
  mutate(signif = ifelse(response == "yP", signif, "Federation")) %>%
  mutate(signif = fct_relevel(signif, "Federation", "Significant")) %>%
  mutate(floor = as_factor(floor)) %>%
  mutate(response = case_match(
    response,
    "y" ~ "Unadjusted",
    "yP" ~ "Participation-adjusted",
    "yPEA" ~ "PEA-adjusted"
  )) %>%
  mutate(response = fct_relevel(response, "Unadjusted", "Participation-adjusted")) %>%
  ggplot() +
  geom_boxplot(aes(x = floor, y = gap, fill = filter), outlier.shape = NA) +
  geom_point(aes(x = floor, y = gap, colour = filter, alpha = signif, shape = signif),
             position = position_jitterdodge(jitter.width = 0.06, seed = 54321)) +
  geom_hline(yintercept = 0, alpha = 0.5, linetype = "dashed") +
  labs(x = "Rating floor", y = "Rating gap (men - women)") +
  facet_grid(metric ~ response, scale = "free_y") +
  scale_colour_manual(name = "",
                      values = rev(c("steelblue","forestgreen","goldenrod","plum3"))) +
  scale_fill_manual(name = "",
                    values = rev(c("white","white","white","white"))) +
  scale_alpha_manual(name = "", values = c(0.7, 1, 0.2)) +
  scale_shape_manual(name = "", values = c(1, 19, 19), guide = "none") +
  guides(colour = guide_legend(nrow = 1, order = 1, override.aes = list(alpha = 1)),
         fill = guide_legend(nrow = 1, order = 1),
         alpha = guide_legend(nrow = 1, order = 2, override.aes = list(shape = c(1, 19, 19)))) +
  theme_bw(base_size = 14) +
  theme(legend.position = "bottom", legend.box = "vertical",
        legend.direction = "vertical",
        legend.spacing.y = unit(0, "lines"), legend.margin = margin(0, 0, 0, 0))
#ggsave("figures/fig_4.pdf", width = 10, height = 8.57)

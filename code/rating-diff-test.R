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
    s %in% 1:2 ~ "significant",
    s == 0 ~ "nonsignificant",
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
  mutate(metric = fct_relevel(metric, "mean", "top10")) %>%
  mutate(filter = str_c("J", 1*juniors, "-I", 1*inactives), .before = 1) %>%
  arrange(juniors, inactives, floor, fed, metric) %>%
  pivot_longer(cols = starts_with("y"), names_to = "response", values_to = "gap") %>%
  mutate(floor = as_factor(floor)) %>%
  ggplot() +
  geom_boxplot(aes(x = floor, y = gap, fill = filter), outlier.shape = NA) +
  geom_point(aes(x = floor, y = gap, colour = filter, alpha = signif),
             position = position_jitterdodge(jitter.width = 0.06, seed = 54321)) +
  labs(x = "rating floor", y = "rating gap (men - women)") +
  facet_grid(response ~ metric) +
  scale_colour_manual(name = "filter: ",
                      values = c("steelblue", "goldenrod", "forestgreen", "plum3")) +
  scale_fill_manual(name = "filter: ",
                    values = c("white", "white", "white", "white")) +
  scale_alpha_manual(name = "", values = c(0.2, 1)) +
  guides(color = guide_legend(override.aes = list(alpha = 1))) +
  theme_bw() +
  theme(legend.position = "bottom")
#ggsave("figures/fig_4_test.pdf", width = 7, height = 6)

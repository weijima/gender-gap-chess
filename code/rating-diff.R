library(tidyverse)

read_csv("data/null-stats.csv", show_col_types = FALSE) %>%
  filter(fed != "ALL", stat == "obs") %>%
  rename(y = value) %>%
  left_join(read_csv("data/age-experience-tab.csv", show_col_types = FALSE),
            by = join_by(metric, juniors, inactives, floor, fed)) %>%
  select(-c(stat, E, A, weight)) %>%
  filter(metric %in% c("mean", "top1", "top10")) %>%
  mutate(metric = fct_relevel(metric, "mean", "top10")) %>%
  mutate(filter = fct_rev(str_c("J", 1*juniors, "-I", 1*inactives)), .before = 1) %>%
  arrange(juniors, inactives, floor, fed, metric) %>%
  pivot_longer(cols = starts_with("y"), names_to = "response", values_to = "gap") %>%
  mutate(response = case_match(response, "y" ~ "raw", "yP" ~ "participation-corrected",
                               "yPEA" ~ "PEA-corrected")) %>%
  mutate(response = fct_relevel(response, "raw", "participation-corrected")) %>%
  ggplot(aes(x = as_factor(floor), y = gap, fill = filter)) +
  geom_boxplot(alpha = 0.8, outlier.alpha = 0.3) +
  labs(x = "rating floor", y = "rating gap (men - women)") +
  facet_grid(metric ~ response, scale = "free_y") +
  scale_fill_manual(name = "filter: ",
                    values = rev(c("steelblue","forestgreen","goldenrod","plum3"))) +
  theme_bw() +
  theme(legend.position = "bottom")
#ggsave("figures/fig_4.pdf", width = 7, height = 6)

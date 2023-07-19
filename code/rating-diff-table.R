library(tidyverse)

read_csv("data/null-stats.csv", show_col_types = FALSE) %>%
  filter(fed != "ALL", stat == "obs") %>%
  rename(y = value) %>%
  left_join(read_csv("data/age-experience-tab.csv", show_col_types = FALSE),
            by = join_by(metric, juniors, inactives, floor, fed)) %>%
  select(-c(stat, E, A, weight)) %>%
  filter(metric %in% c("mean", "top1", "top10")) %>%
  mutate(filter = str_c("J", 1*juniors, "-I", 1*inactives), .before = 1) %>%
  arrange(juniors, inactives, floor, fed, metric) %>%
  pivot_longer(cols = starts_with("y"), names_to = "response", values_to = "gap") %>%
  ggplot(aes(x = as_factor(floor), y = gap, fill = filter)) +
  geom_boxplot() +
  labs(x = "rating floor", y = "rating gap (men - women)") +
  facet_grid(response ~ metric) +
  scale_fill_manual(name = "filter: ",
                    values = c("steelblue", "goldenrod", "forestgreen", "plum3")) +
  theme_bw() +
  theme(legend.position = "bottom")
#ggsave("figures/fig_4_test.pdf", width = 7, height = 6)

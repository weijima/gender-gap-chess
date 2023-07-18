library(tidyverse)

read_csv("data/null-stats.csv", show_col_types = FALSE) %>%
  filter(stat == "obs") %>%
  mutate(value = case_when( # Adjust for different sign convention (F - M to M - F)
    stat %in% c("obs", "ptmean") ~ -value,
    stat == "ptpval" ~ 1 - value,
    .default = value
  )) %>%
  rename(y = value) %>%
  left_join(read_csv("data/age-experience-tab.csv", show_col_types = FALSE),
            by = join_by(metric, juniors, inactives, floor, fed)) %>%
  filter(metric %in% c("mean", "top1", "top10")) %>%
  mutate(filter = str_c("J", 1*juniors, "-I", 1*inactives), .before = 1) %>%
  select(-c(stat, E, A, weight)) %>%
  relocate(fed, .before = metric) %>%
  arrange(juniors, inactives, floor, fed, metric) %>%
  pivot_longer(cols = starts_with("y"), names_to = "adj", values_to = "gap") %>%
  ggplot(aes(x = as_factor(floor), y = gap, fill = filter)) +
  geom_boxplot() +
  labs(x = "rating floor", y = "rating gap (men - women)") +
  facet_grid(adj ~ metric) +
  scale_fill_manual(name = "filter: ",
                    values = c("steelblue", "goldenrod", "forestgreen", "plum3")) +
  theme_bw() +
  theme(legend.position = "bottom")

library(tidyverse)


plot_age_inact <- function(data_inact) {
  data_inact %>%
    mutate(age = 2019L - as.integer(born) - 1L) %>%
    filter(age <= 80) %>%
    filter(!(year == 2012 & month == 10)) %>%
    mutate(sex = fct_relevel(ifelse(sex == "F", "Women", "Men"), "Women", "Men")) %>%
    ggplot(aes(x = age, colour = sex, fill = sex)) +
    geom_density(alpha = 0.3) +
    scale_colour_viridis_d(option = "C", end = 0.8) +
    scale_fill_viridis_d(option = "C", end = 0.8) +
    labs(x = "Age at becoming inactive", y = "Density") +
    theme_minimal() +
    theme(panel.grid = element_blank(),
          axis.line = element_blank(),
          axis.ticks = element_line(colour = "grey80"),
          panel.border = element_rect(colour = "grey80", fill = NA),
          panel.background = element_blank(),
          legend.title = element_blank(),
          legend.position = "bottom",
          strip.background = element_blank(),
          strip.placement = "outside")
}


inactive_ids <-
  read_csv("data/rating-data.csv", col_types = "cccdiil") %>%
  filter(!active) %>%
  select(id)


data_inact <-
  tibble(file = Sys.glob("data/tmp/standard_*.rds")) %>%
  mutate(data = map(file, read_rds, .progress = TRUE)) %>%
  unnest(data) %>%
  select(!file) %>%
  right_join(inactive_ids, by = join_by(id)) %>%
  arrange(year, month) %>%
  filter(born != "0000" & born != "0") %>%
  filter(!is.na(flag))


data_inact %>%
  slice_head(n = 1, by = id) %>%
  plot_age_inact()
#ggsave("figures/dropout.pdf", width = 4, height = 3.4)


data_inact %>%
  mutate(time = 12L * (year - 2012L) + month) %>%
  mutate(diff = time - lag(time), .by = id) %>%
  drop_na() %>%
  filter(diff > 1) %>%
  slice_tail(n = 1, by = id) %>%
  plot_age_inact()
#ggsave("figures/dropout2.pdf", width = 4, height = 3.4)

library(tidyverse)


gapFractionExplained <- function(isJunior, isInactive, obsMetric, ratingFloor, col, dat){
  dat %>%
    filter(juniors == isJunior & inactives == isInactive & floor == ratingFloor &
             metric == obsMetric) %>%
    summarize(y = mean(y), yAdj = mean({{col}})) %>%
    mutate(frac = 1 - yAdj / y) %>%
    pull(frac)
}


dat <- read_csv("data/null-stats.csv", show_col_types = FALSE) %>%
  filter(fed != "ALL", stat == "obs") %>%
  rename(y = value) %>%
  left_join(read_csv("data/age-experience-tab.csv", show_col_types = FALSE),
            by = join_by(metric, juniors, inactives, floor, fed)) %>%
  select(-c(stat, E, A, weight)) %>%
  filter(metric %in% c("mean", "top1", "top10"))


crossing(isJunior = c(FALSE, TRUE),
         isInactive = c(FALSE, TRUE),
         obsMetric = c("top10", "top1"),
         ratingFloor = c(1000, 1400, 1600)) %>%
  mutate(frac = pmap_dbl(., gapFractionExplained, col = yP, dat = dat)) %>%
  print(n = Inf)

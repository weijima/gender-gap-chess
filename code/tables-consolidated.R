library(tidyverse)


participation_gap <- function(rating_data) {
  rating_data %>%
    count(fed, sex, name = "no_of_players") %>%
    pivot_wider(names_from = "sex", values_from = "no_of_players", values_fill = 0) %>%
    mutate(
      percent_women = str_c(sprintf("%.1f", round(100 * `F` / (`F` + `M`), 1)), "\\%")
    )
}

restrict_data <- function(juniors, inactives, floor,
                          rating_data, birth_uncertain = FALSE) {
  if (juniors) max_byear <- 2019 else max_byear <- 1999
  rating_data %>%
    filter(if (inactives) TRUE else active) %>%
    filter(if (birth_uncertain) TRUE else !is.na(born)) %>%
    filter(born <= max_byear | is.na(born), rating >= floor)
}


federationTable <- function(rating_data, jun, inact, ratfloor) {
  read_csv("data/null-stats.csv", show_col_types = FALSE) %>%
    filter(fed != "ALL" & metric %in% c("mean", "top10", "top1") & stat != "ptsd") %>%
    filter(juniors == jun & inactives == inact & floor == ratfloor) %>%
    select(!juniors & !inactives & !floor) %>%
    pivot_wider(names_from = c(metric, stat), values_from = value) %>%
    left_join(
      rating_data %>%
        restrict_data(juniors = jun, inactives = inact, floor = ratfloor) %>%
        participation_gap() %>%
        filter(`M` >= 30 & `F` >= 30),
      by = join_by(fed)
    ) %>%
    relocate(fed, `M`, `F`, percent_women, mean_obs, mean_ptmean, mean_ptpval,
             top10_obs, top10_ptmean, top10_ptpval,
             top1_obs, top1_ptmean, top1_ptpval) %>%
    mutate(across(ends_with("obs") | ends_with("ptmean"), round)) %>%
    mutate(across(ends_with("ptpval"), \(x) round(x, 4))) %>%
    mutate(across(ends_with("ptpval"), \(x) {
      ifelse(x == 0, "< 10\\textsuperscript{--4}", sprintf("%.4f", x))
    } )) %>%
    arrange(desc(percent_women))
}



crossing(jun = c(TRUE, FALSE),
         inact = c(TRUE, FALSE),
         ratfloor = c(1000, 1400, 1600)) %>%
  mutate(rating_data = list(read_csv("data/rating-data.csv", col_types = "ccciiil")),
         .before = 1) %>%
  mutate(table = pmap(., federationTable)) %>%
  mutate(table = map(table, \(x) knitr::kable(x, format = "latex", escape = FALSE))) %>%
  mutate(fname = str_c("data/federation-tables/fedtab-",
                       ifelse(jun, "", "no"), "juniors-",
                       ifelse(inact, "", "no"), "inactives-",
                       ratfloor, ".tex")) %>%
  mutate(table = walk2(table, fname, write_lines))

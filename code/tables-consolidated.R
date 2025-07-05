library(tidyverse)


participation_gap <- function(rating_data) {
  rating_data %>%
    count(fed, sex, name = "no_of_players") %>%
    pivot_wider(names_from = "sex", values_from = "no_of_players", values_fill = 0) %>%
    mutate(
      percent_women = str_c(sprintf("%.1f", round(100 * `F` / (`F` + `M`), 1)), "\\%")
    )
}

restrict_data <- function(rating_data, juniors, inactives, floor,
                          birth_uncertain = FALSE) {
  if (juniors) max_byear <- 2019 else max_byear <- 1999
  rating_data %>%
    filter(if (inactives) TRUE else active) %>%
    filter(if (birth_uncertain) TRUE else !is.na(born)) %>%
    filter(born <= max_byear | is.na(born), rating >= floor)
}


federationTable <- function(null_stats, rating_data, juniors, inactives, floor) {
  null_tab <- null_stats %>%
    filter(fed != "ALL" & metric %in% c("mean", "top10", "top1") & stat != "ptsd") %>%
    filter(juniors == {{juniors}} & inactives == {{inactives}} & floor == {{floor}}) %>%
    select(!juniors & !inactives & !floor) %>%
    pivot_wider(names_from = c(metric, stat), values_from = value)
  rating_tab <- rating_data %>%
    restrict_data({{juniors}}, {{inactives}}, {{floor}}) %>%
    participation_gap() %>%
    filter(`M` >= 30 & `F` >= 30)
    left_join(null_tab, rating_tab, by = join_by(fed))
}


fedTableForOutput <- function(fedTable) {
  fedTable %>%
    relocate(fed, `M`, `F`, percent_women, mean_obs, mean_ptmean, mean_ptpval,
             top10_obs, top10_ptmean, top10_ptpval,
             top1_obs, top1_ptmean, top1_ptpval) %>%
    mutate(across(ends_with("obs") | ends_with("ptmean"), round)) %>%
    mutate(across(ends_with("ptpval"), \(x) round(x, 4))) %>%
    mutate(across(ends_with("ptpval"), \(x) {
      ifelse(x == 0, "$<$ 10\\textsuperscript{--4}", sprintf("%.4f", x))
    } )) %>%
    arrange(desc(percent_women))
}



crossing(juniors = c(TRUE, FALSE),
         inactives = c(TRUE, FALSE),
         floor = c(1000, 1400, 1600)) %>%
  mutate(null_stats = list(read_csv("data/null-stats.csv", col_types = "llicccd")),
         rating_data = list(read_csv("data/rating-data.csv", col_types = "ccciiil")),
         .before = 1) %>%
  mutate(table = pmap(., compose(fedTableForOutput, federationTable))) %>%
  mutate(table = map(table, \(x) knitr::kable(x, format = "latex", escape = FALSE))) %>%
  mutate(fname = str_c("data/federation-tables/fedtab-",
                       ifelse(juniors, "", "no"), "juniors-",
                       ifelse(inactives, "", "no"), "inactives-", floor, ".tex")) %>%
  mutate(table = walk2(table, fname, write_lines))

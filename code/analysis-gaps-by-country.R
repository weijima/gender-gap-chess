library(magrittr)
library(data.table)
library(ggplot2)
library(foreach)
library(doParallel)
registerDoParallel(cores=4)

chess <- readr::read_csv("data/20201006_FIDE_ratings.csv")
inactive <- readr::read_csv("data/inactive_players_jan20.txt", col_names = "fideid") %>%
  dplyr::mutate(inactive = TRUE)

combined <- dplyr::full_join(chess, inactive)

# set parameters
n_highest = 10
n_draws = 100000

# filter datasets
chess_filtered_world <- combined %>%
  dplyr::mutate(inactive = ifelse(is.na(inactive), FALSE, TRUE)) %>%
  dplyr::filter(birthday < 2000, 
                inactive == FALSE)



regions <- c("RUS", "GER", "IND", "ESP", "FRA", "POL", "ITA", "IRI", "CZE", 
             "TUR", "USA", "HUN", "GRE", "BRA", "SRB", "NED", "ARG", "UKR", 
             "DEN", "SVK")


summary <- purrr::map_dfr(regions, 
               .f = function (region) {
                  chess_filtered_world %>%
                   dplyr::filter(country == region) %>%
                   dplyr::group_by(sex) %>%
                   dplyr::summarise(number = max(dplyr::n()), 
                                    mean = mean(rating), 
                                    median = median(rating), 
                                    sd = sd(rating), 
                                    .groups = "drop_last") %>%
                   dplyr::mutate(country = region)
               })

summary_top <- purrr::map_dfr(regions, 
                              .f = function (region) {
                                chess_filtered_world %>%
                                  dplyr::filter(country == region) %>%
                                  dplyr::group_by(sex) %>%
                                  dplyr::arrange(sex, rating) %>%
                                  dplyr::slice_tail(n = n_highest) %>%
                                  dplyr::summarise(number = max(dplyr::n()), 
                                                   mean = mean(rating), 
                                                   median = median(rating), 
                                                   sd = sd(rating), 
                                                   .groups = "drop_last") %>%
                                  dplyr::mutate(country = region)
                              })



permute <- function(scores_female, scores_male, n_highest = 10) {
  n_small <- length(scores_female)
  n_large <- length(scores_male)
  n_total <- n_small + n_large
  
  all_scores <- c(scores_female, scores_male)
  
  # shuffle scores
  all_scores <- all_scores[sample(n_total)]
  
  # draw small and large sample
  values_small <- all_scores[1:n_small]
  values_large <- all_scores[(n_small + 1):n_total]
  
  
  if (n_highest == 1) {
    mean_small <- max(values_small)
    mean_large <- max(values_large)
  } else {
    mean_small <- mean(tail(sort(values_small), n_highest))
    mean_large <- mean(tail(sort(values_large), n_highest))
  }
  
  
  return(mean_large - mean_small)
}


apply_permuatation <- function(region) {
  scores_female <- chess_filtered_world %>%
    dplyr::filter(country == region, 
                  sex == "F") %>%
    dplyr::pull(rating)
  
  scores_male <- chess_filtered_world %>%
    dplyr::filter(country == region, 
                  sex == "M") %>%
    dplyr::pull(rating)
  
  observed_difference <- chess_filtered_world %>%
    dplyr::filter(country == region) %>%
    dplyr::group_by(sex) %>%
    dplyr::arrange(rating) %>%
    dplyr::slice_tail(n = n_highest) %>%
    dplyr::summarise(mean = mean(rating), 
                     .groups = "drop_last") %>%
    dplyr::pull(mean) %>%
    diff()
  
  permuted_differences <- replicate(n = n_draws, expr = {
    permute(scores_female, scores_male, n_highest = n_highest)
  })
  
  res <- data.frame(country = region, 
                    observed_difference = observed_difference, 
                    mean_perm_difference = mean(permuted_differences), 
                    perc_smaller = mean(permuted_differences <= observed_difference),
                    n_highest = n_highest)
  
  return(res)
  
}

n_highest = 1

results <- foreach(i = 1:length(regions), .combine = 'rbind') %dopar% {
  apply_permuatation(regions[i])
}








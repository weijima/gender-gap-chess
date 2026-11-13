clargs <- commandArgs(trailingOnly=TRUE)
if (length(clargs) > 0) {
  rownum <- as.integer(clargs[1])
  perms <- as.integer(clargs[2])
} else {
  rownum <- 1L
  perms <- 100L
}

library(tidyverse)
library(Rcpp)

sourceCpp("code/permutation-fast.cpp")

# Use as many threads as slurm allocates (--cpus-per-task); 1 if not set
RcppParallel::setThreadOptions(numThreads = as.integer(Sys.getenv("SLURM_CPUS_PER_TASK", "1")))

restrict_data <- function(rating_data, include_junior, include_inactive, min_rating,
                          birth_uncertain = FALSE) {
  if (include_junior) max_byear <- 2019 else max_byear <- 1999
  rating_data %>%
    filter(if (include_inactive) TRUE else active) %>%
    filter(if (birth_uncertain) TRUE else born != 0) %>%
    filter(born <= max_byear | is.na(born), rating >= min_rating)
}

perm_generator <- function(rating_data, juniors, inactives, floor, metric, perms) {
  rating_data %>%
    restrict_data(juniors, inactives, floor) %>%
    select(sex, rating) %>%
    pivot_wider(names_from = sex, values_from = rating, values_fn = list) %>%
    mutate(permuts = map2(`M`, `F`, function(m, f) permut_tab_fast(m, f, metric, perms)))
}


rating_data <- read_csv("data/rating-data.csv", col_types = "cccdiil")
dir.create("data/permdat", showWarnings = FALSE, recursive = TRUE)

crossing(juniors = c(FALSE, TRUE),
         inactives = c(FALSE, TRUE),
         floor = seq(1000, 1800, by = 50),
         metric = c("mean", "median", "sd", "top1", "top10")) %>%
  slice(rownum) %>%
  mutate(results = pmap(list(juniors, inactives, floor, metric),
                        perm_generator, rating_data = rating_data, perms = perms)) %>%
  unnest(results) %>%
  write_rds(str_c("data/permdat/perm-data-global-1e6-", rownum, ".rds"), compress = "gz")

library(tidyverse)


# Table of file names with null results:
tibble(file = Sys.glob("data/nulls/nulls-*.csv")) %>%
  # Extract file names, without path and extension:
  mutate(name = fs::path_ext_remove(fs::path_file(file))) %>%
  # For consistency, tag "-R1000" to file names with no rating floor:
  mutate(name = if_else(str_count(name, "-") == 2L, str_c(name, "-R1000"), name)) %>%
  # Separate out these names by the dash ("-") into different columns:
  separate(name, into = c("tag", "include_junior", "include_inactive", "rating_floor"),
           sep = "-") %>%
  # Convert "junior" and "inactive" into logical, and ratingFloor into integer:
  mutate(include_junior = if_else(include_junior == "J1", TRUE, FALSE)) %>%
  mutate(include_inactive = if_else(include_inactive == "I1", TRUE, FALSE)) %>%
  mutate(rating_floor = as.integer(str_remove(rating_floor, "R"))) %>%
  # Now read in all data files:
  mutate(data = map(file, read_csv, show_col_types = FALSE)) %>%
  # Standardize column names in sub-tables to be lowercase:
  mutate(data = map(data, ~rename_with(.x, tolower))) %>%
  unnest(data) %>%
  # Remove unnecessary columns:
  select(-file, -tag) %>%
  # Tidy up the data:
  pivot_longer(!include_junior & !include_inactive & !rating_floor & !fed,
               names_to = "metric") %>%
  # Save data:
  write_rds("data/nulls/nulls.rds", compress = "xz")

# obs: observed value, e.g. diff. between top 1 mean ratings in the actual data
# ptmean, ptsd, ptpval: mean, std dev, and p-value under the permutation tests

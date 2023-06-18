library(tidyverse)


# Table of file names with null results:
tibble(file = Sys.glob("data/nulls/nulls-*.csv")) %>%
  # Extract file names, without path and extension:
  mutate(name = fs::path_ext_remove(fs::path_file(file))) %>%
  # For consistency, tag "-R1000" to file names with no rating floor:
  mutate(name = if_else(str_count(name, "-") == 2L, str_c(name, "-R1000"), name)) %>%
  # Separate out these names by the dash ("-") into different columns:
  separate_wider_delim(name, delim="-", names=c(NA, "juniors", "inactives", "floor")) %>%
  # Convert "juniors" and "inactives" to logical, and "floor" to integer:
  mutate(juniors = juniors == "J1",
         inactives = inactives == "I1",
         floor = as.integer(str_remove(floor, "R"))) %>%
  # Now read in all data files:
  mutate(data = map(file, read_csv, show_col_types = FALSE)) %>%
  # Standardize column names in sub-tables to be lowercase:
  mutate(data = map(data, ~rename_with(.x, tolower))) %>%
  unnest(data) %>%
  # Remove the now-unnecessary "file" column:
  select(-file) %>%
  # Tidy up the data:
  pivot_longer(!juniors & !inactives & !floor & !fed, names_to = "metric") %>%
  # Have "top1" and "top10" as metrics, instead of "max1" and "max10":
  mutate(metric = str_replace(metric, "max", "top")) %>%
  # Save data:
  write_rds("data/nulls/nulls.rds", compress = "xz")

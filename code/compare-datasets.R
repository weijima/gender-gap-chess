library(tidyverse)


# Load Richard's dataset:
read_csv("data/large_data/ratings20191231_downloaded20211001.csv",
         show_col_types = FALSE) %>%
  # Rename columns to match those in my dataset:
  rename(id = fideid, fed = country, born = birthday, ratingRS = rating) %>%
  # Change column types to match mine:
  mutate(id = as.character(id)) %>%
  # And change NA in birth year to 0 (again, to match my setup):
  mutate(born = if_else(is.na(born), 0L, as.integer(born))) %>%
  # Change NA flag values to "m" (any character without an "i", for "inactive", will do):
  mutate(flag = if_else(is.na(flag), "m", flag)) %>%
  # Create a logical column called "active", based on whether "flag" contains an "i":
  mutate(active = str_detect(flag, "i", negate = TRUE)) %>%
  # In 11 cases, sex was NA. In all those cases, the players are male. Fixing this:
  mutate(sex = if_else(is.na(sex), "M", sex)) %>%
  # Keep only the relevant columns:
  select(id, fed, sex, ratingRS, born, active) %>%
  # One player is missing from my dataset compared with Richard's (male player from
  # (the UAE, rated 1812 and inactive). Remove this row:
  filter(id != "9301879") %>%
  # Now read in my data and join with Richard's. The resulting table has just as many
  # rows as the two joined ones, indicating that they line up perfectly:
  left_join(read_csv("data/rating-data.csv", col_types = "cccdiil") %>%
              rename(ratingGB = rating) %>%
              select(-games),
            by = join_by(id, fed, sex, born, active)) %>%
  # Reorder the columns:
  relocate(id, fed, sex, born, active, ratingRS, ratingGB) %>%
  # Now we show that the datasets are identical, by counting how many rows there are
  # where the ratings are different:
  filter(ratingRS != ratingGB) %>%
  nrow()


# Compare Richard's permutation results with mine:
read_csv("data/nulls-Richard/null-stats-Richard.csv", show_col_types = FALSE) %>%
  # Restrict to permutation means, std devs, p-values, and observed differences:
  filter(str_detect(metric, "(_pt|obs)")) %>%
  separate_wider_delim(metric, delim = "_", names = c("metric", "stat")) %>%
  # Get rid of the "ALL" federation (i.e., all federations together):
  filter(fed != "ALL") %>%
  # Merge the data with my permutation results:
  left_join(read_csv("data/null-stats.csv", show_col_types = FALSE) %>%
              rename(myvalue = value), # Rename value column, to compare with Richard's
            by = join_by(juniors, inactives, floor, metric, fed, stat)) %>%
  # Adjust my values for different conventions (F - M to M - F):
  mutate(myvalue = case_when(
    stat %in% c("obs", "ptmean") ~ -myvalue,
    stat == "ptpval" ~ 1 - myvalue,
    .default = myvalue
  )) %>%
  # Differences between Richard's and my results:
  mutate(diff = value - myvalue) %>%
  # Create plot:
  ggplot(aes(x = value, y = diff)) +
  geom_point(colour = "steelblue", alpha = 0.25) +
  labs(x = "Richard's values", y = "Difference between our values") +
  facet_wrap(~stat, scales = "free") +
  theme_bw()

library(tidyverse) # Packages for efficient data manipulation and plotting


# Fix a particular problem in the raw data files, where the text "SIAGM" pushes the
# ratings too far to the right, violating the fixed-width structure of the data. The
# input "datafile" is a string with the path and name of the .txt data file. This
# function modifies this file in-place (so its use requires care).
fix_parsing <- function(datafile) {
  # Replace every occurrence of "SIAGM" with just "SIM", which fixes line width issue:
  system(str_c("gsed -i 's/SIAGM/SIM/g' ", datafile)) # Use "sed" (Unix) to edit file
}


# Download a set of FIDE data files into the directory specified by "exdir",
# in .zip format. All data files are downloaded for each year & month specified in
# the data frame "month_year", which has two columns: "month" (with three-letter
# month abbreviations) and "year" (year minus 2000; so 18 means 2018).
download_data <- function(month_year, exdir) {
  month_year %>% # Data frame with combinations of months and years
    # From the above, generate download URLs and destination names to save the files to:
    transmute(download = str_c("https://ratings.fide.com/download/standard_",
                               month, year, "frl.zip"),
              destfile = str_c(normalizePath(exdir), "/zip_", month, year, ".zip")) %>%
    # Now download each file in turn:
    mutate(walk2(download, destfile, download.file))
}


# Extract, convert, and merge all downloaded data in the directory "exdir".
gather_data <- function(exdir) {
  tibble(zipfile = Sys.glob(str_c(exdir, "/*.zip"))) %>% # Zip file names in a column
    mutate(walk2(zipfile, exdir, ~unzip(.x, exdir = .y))) %>% # Extract zip files
    mutate(datafile = unique(Sys.glob(str_c(exdir, "/*.txt")))) %>% # Data file names
    mutate(walk(datafile, fix_parsing)) %>% # Fix fwf parsing issues in data
    mutate(outfile = str_replace(datafile, ".txt", ".rds")) %>% # Rds file names
    mutate(walk2(datafile, outfile, make_rds)) %>% # Convert to rds and save
    transmute(exdir, dat = map(outfile, read_rds)) %>% # Read in all those rds files
    unnest(dat) # Merge separate data tables into single large table
}


# It is difficult to read the headers of fixed-width files (fwf). This function does
# that: it loads the first line of the .txt data file "datafile", and returns a vector
# of extracted column names.
create_header <- function(datafile) {
  read_lines(datafile, n_max = 1) %>% # Read just the header line of file
    str_replace("ID Number", "id") %>% # Change "ID Number" to only "id"
    str_replace("B-day", "born") %>% # Change "B-day" to "born"
    str_squish() %>% # Replace repeated spaces with just one space everywhere in string
    str_split(" ") %>% # Split the string by the spaces
    unlist() %>% # Convert to vector by dropping list structure
    map_chr(tolower) # Convert all column names to purely lowercase characters
}


# Read a .txt data file ("datafile") and save it as an .rds file ("outfile").
make_rds <- function(datafile, outfile) {
  header <- create_header(datafile)
  # The rating data are not in a column called "rating". Instead, they are in a column
  # named by the month and the year; e.g., "may18". We extract which entry of the
  # header vector has numbers in it (because the ratings are the only such column):
  index <- which(str_detect(header, "[:digit:]")) # Which column has the ratings?
  monthyear <- header[index] # Extract month & year (e.g., "may18")
  month <- str_sub(monthyear, 1, 3) # Extract month (e.g., "may" from "may18")
  month <- which(tolower(month.abb) == month) # Convert to number (e.g., 5 for "may")
  year <- as.integer(str_sub(monthyear, 4, 5)) # Extract year (e.g., 18 from "may18")
  header[index] <- "rating" # Rename column with ratings from e.g. "may18" to "rating"
  read_fwf(datafile, fwf_empty(datafile, col_names = header)) %>% # Load file
    slice(-1) %>% # Drop 1st row (header info; used above to set all column types to chr)
    select(id, fed, sex, rating, gms, born, flag) %>% # Keep only these columns
    mutate(month = month, year = 2000L + year) %>% # Add month and year as columns
    write_rds(outfile) # Write file to disk
}


# Clean up the extracted data. The argument "data" is a data frame, produced by
# "make_rds". The function returns a similar data frame but more streamlined.
clean_data <- function(data) {
  arrange(data, year, month, fed, sex) %>%
    mutate(born = if_else(born == "0000", "0", born)) %>% # Fix "born" column
    mutate(born = as.integer(born)) %>% # Now it can be converted to integer
    filter(!is.na(born)) %>% # This drops 18 rows with additional parsing problems!
    mutate(rating = as.numeric(rating), gms = as.integer(gms)) %>%
    mutate(active = !(flag %in% c("i", "wi"))) %>% # Player active?
    select(-flag) %>% # Remove "flag" (because it is superseded by "active" above)
    rename(games = gms) # Rename the column "gms" to "games"
}


# Add up the total number of games played by each player, and add it as the updated
# "games" column to the main data.
sum_games <- function(rating_data) {
  rating_data %>%
    group_by(id) %>%
    mutate(games = sum(games)) %>%
    ungroup() %>%
    filter(year == max(year)) %>% # Filter for latest year in data
    filter(month == max(month)) %>% # Within that year, filter for latest month
    select(-month, -year)
}



# Download, merge, clean, and save data, then clean up downloaded files:
tibble(exdir = fs::path_abs("data/tmp/"), # ASSUMES WORKING DIRECTORY IS: /code
       month_year = list( # All month-year combinations, from 2012 Oct to 2020 Jan:
         crossing(month = tolower(month.abb), year = 12:19) %>%
           filter(year > 12 | month %in% c("oct", "nov", "dec")) %>%
           add_row(month = "jan", year = 20) # Add 2020 Jan manually to the end
       )) %>%
  # Download data:
  mutate(walk(exdir, ~system(str_c("mkdir -p ", .x)))) %>% # Create temporary folder
  mutate(walk2(month_year, exdir, download_data)) %>%
  # Merge data:
  pull(exdir) %>%
  gather_data() %>%
  # Data cleaning:
  group_nest(exdir) %>%
  mutate(data = map(data, clean_data)) %>%
  # Save cleaned data:
  mutate(walk2(data, exdir,
               ~write_rds(.x, str_c(.y, "/raw_data.rds"), compress = "xz"))) %>%
  # Remove all downloaded & temporary files:
  mutate(walk(exdir, ~file.remove(Sys.glob(str_c(.x, "/stand*"))))) %>% # txt & rds files
  mutate(walk(exdir, ~file.remove(Sys.glob(str_c(.x, "/*.zip"))))) # zip files

# Move file from /data/tmp to /data:
system(str_c("mv data/tmp/raw_data.rds data/raw_data.rds"))
system(str_c("rmdir data/tmp")) # Remove temporary directory

# Add column with number of games played from 2012 Oct to 2020 Jan:
read_rds("data/raw_data.rds") %>% # Load large data file created above
  filter(year != 2020) %>% # Comment out to include 2020 January from the data as well
  sum_games() %>%
  write_rds("data/rating_data.rds", compress = "xz")

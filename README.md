## Data and code for the manuscript "Deconstructing the gender gap in chess ratings" (in prep).

This project aims at a rigorous and comprehensive analysis of gender differences in the ratings of top-level chess players. The *participation rate hypothesis* states that the naively-observed gender difference between top-rated chess players might largely be a statistical artifact: since only about 10% of rated players are women, the tail end of their rating distribution is naturally undersampled compared to that of men. We quantify whether, and to what extent, the participation rate hypothesis explains observed rating differences between genders. We do so using a robust nonparametric statistical approach and a comprehensive, global dataset of over 340,000 internationally-rated chess players coming from 99 national federations.


#### Software dependencies: R (tested with R 4.2.1 and 4.2.2). Required R packages:

* `tidyverse`: Efficient data manipulation and plotting
* `Rcpp`: Importing and compiling functions written in C++
* `fs`: Cross-platform file system operations

Required non-standard hardware: none. However, reproducing the permutation results with sufficiently many permutations (more than a hundred thousand per sample) does benefit from having access to a computing cluster.


#### Contents of the `code` directory

* `age-experiance-figure.R`: Create a figure comparing the experience level (in terms of mean number of games played) and age between women and men, across all data filters.
* `compare-Richard-Gyuri-data.R`: Making sure that the data which were independently collected and analyzed by different team members do line up.
* `create-figures.R` and `create-figures-test.R`: Scripts generating histograms and scatterplots of rating differences corrected for participation, age, and experience.
* `download-and-clean.R`: Automatically download and process the FIDE rating data. The final output consists of two files in the `data` directory: `raw-data.rds` (with data for all months between October 2012 and December 2019), and `rating-data.csv` (with data just for December 2019 - however, with the `games` column containing the total number of games played from Oct 2012 to Dec 2019).
* `generate-perm-table-cluster.R`: A script that will create rating permutations of the ratings of women and men, for each federation, and for a given parameterization. Here "parameterization" means 1) whether junior players (those born after 1999) are excluded, 2) whether inactive players are excluded, 3) whether only players rated below a certain rating floor are excluded (the options are 1000, 1400, or 1600), and 4) which metric is to be calculated (mean, median, sd, top1, or top10). The purpose of this script is to act as one that can be submitted to a large computing cluster. It takes two command-line input parameters:
  - `rownum`: an integer between 1 and 60 (inclusive), determining which combination of the above parameters will be implemented.
  - `perms`: a positive integer, setting the number of random permutations of a given federation's ratings to be generated. For example, if `perms` is `100`, then the ratings for women and men of any given federation will be randomly reshuffled 100 times, and the assigned metric (mean, median, sd, top1, or top10) calculated over those permutations. (*Note:* in practice, at least one million permutations are required for all parameterizations to reach reasonable convergence.)
* `generate-perm-table.R`: As `generate-perm-table-cluster.R`, but performing the permutations for all parameterizations all at once. This means that it can conveniently be used locally, without having access to a computing cluster. (*Warning:* running this script on standard hardware is only feasible with no more than 100,000 permutations. Even that could take over a day to finish.)
* `global-figures.R`: Generate histograms of the global rating distributions broken down by gender, for various data filters.
* `global-stats.R`: Various statistical tests and analyses performed on the global data (i.e., those not broken down by individual federations).
* `launch.R`: A script that automatically creates jobs on a computing cluster and submits them, using the `slurm` scheduler. Parameters such as the account name and loaded modules might need to be changed to account for the particular computing cluster setup available to the user.
* `merge-nulls-Richard.R`: Merging the permutation ("null") runs obtained by Richard Smith (one of our team members) into a single, tidy CSV file.
* `per-federation-analysis.R`: For each chess federation, use the permutation results to see whether one can reject the null hypothesis that observed rating differences between women and men are simply due to a difference in participation.
* `permutation-table.cpp`: Two simple C++ functions, implemented this way to glean more speed. The first function, `top10`, simply takes the average of the ten largest entries from a numerical vector. The second function, `permut_tab`, generates the permutations which are at the heart of the scripts `generate-perm-table.R` and `generate-perm-table-cluster.R`.
* `rating-diff.R`: Obtain the raw, participation-corrected, and participation-, experience-, and age-corrected rating gaps. Results obtained for each federation and parameterization. Currently the script creates a plot, summarizing results across federations via box plots for each parameterization. It also creates two table-like figures (one for global, one for per-federation data) showing what fraction of the observed gender gap is explained by the participation rate hypothesis.
* `stats-from-nulls.R`: This script processes the output generated by `generate-perm-table-cluster.R` (which tends to be massive, in case the number of permutations is large), and saves them in a single CSV file.
* `tables-consolidated.R`: Create LaTeX tables summarizing the gender gap with various assumptions, for each federation.
* `test-corr`: How are one's rating and experience (number of games played) related to one another?
* `weighted-regression.R`: Predicting the (participation-corrected) rating difference between women and men per federation, using age and experience as the predictors (without interaction effects). Experience is defined here as the number of games played by a player between Oct 2012 and Dec 2019 (which is the extent of the data available from FIDE). The regression is weighted, with the weights being the inverse variances of the permutation results per federation.
* `perm-test-global`: A directory with versions of the scripts acting on the rating data globally; i.e., by disregarding information on federation. Instead, all players are lumped into one pool for analysis.


#### Contents of the `data` directory

* `null-stats.csv`: The output from `stats-from-nulls.R`. This table has the following columns:
  - `juniors`: `TRUE` or `FALSE`, depending on whether players born after 1999 are included (`TRUE`) or excluded (`FALSE`)
  - `inactives`: `TRUE` or `FALSE`, depending on whether inactive players are included (`TRUE`) or excluded (`FALSE`)
  - `floor`: rating floor (either 1000, 1400, or 1600); players rated below this threshold are excluded
  - `metric`: the metric name (mean, median, sd, top1, or top10)
  - `fed`: three-letter federation code
  - `stat`: what statistic was measured (`obs`: observed difference between women and men in the data; `ptmean`: difference in permutation means; `ptsd`: difference in permutation standard deviations; `ptpval`: the permutation p-value - that is, the fraction of permutation nulls that fell below the observed difference)
  - `value`: the actual value to the metric-statistic combination
* `null-stats-global.csv`: Output from `/code/perm-test-global/stats-from-nulls-global.R`. It has the same structure as `null-stats.csv`, except the federation is always set to `ALL`, to signify that these permutation data were obtained for all players without considering which federation they belong in.
* `age-experience-tab.csv`: The data from the weighted regression, generated by `weighted-regression.R`. The columns are:
  - `metric`: mean, median, sd, top1, or top10
  - `juniors`: `TRUE` or `FALSE`, depending on whether players born after 1999 are excluded
  - `floor`: rating floor (either 1000, 1400, or 1600); players rated below this threshold are excluded
  - `fed`: three-letter federation code
  - `yP`: participation-corrected rating difference (men minus women)
  - `yPEA`: rating difference corrected for participation, age differences, and experience differences
  - `E`: regression coefficient for experience
  - `A`: regression coefficient for age
  - `weight`: inverse variances of the permutation results, used as weights in the weighted regression
* `rating-data.csv`: The cleaned FIDE dataset, generated by `download-and-clean.R`. Its columns are:
  - `id`: unique player ID
  - `fed`: three-letter code of the federation the player belongs to
  - `sex`: `F` or `M`, depending on the sex of the player
  - `rating`: the player's rating in December 2019
  - `games`: the number of games played by the player between Oct 2012 and Dec 2019 (summed up using all the monthly FIDE rating lists in between)
  - `born`: the year in which the player was born; unknown or corrupted values are all replaced by `0`
  - `active`: `TRUE` if the player was still active as of December 2019; `FALSE` otherwise
* `raw-data.rds`: A compressed table with all FIDE data between Oct 2012 and Dec 2019. It is also generated by `download-and-clean.R`, and its structure is almost the same as for `rating-data.csv`. The differences are: there are two extra columns (month and year, at which the data were published); the `games` column means the number of games played in just the corresponding month and year; and the same player ID may appear many times (documenting a player's rating and number of games played in each month and year).
* `rating-data-Richard.csv`: The rating data compiled and used by Richard Smith.
* `federation-tables`: A directory holding the LaTeX tables generated by `tables-consolidated.R`.


#### Contents of the `literature` directory

Various related articles, both academic (in the sub-directory `academic`) and other (in the sub-directory `other`).

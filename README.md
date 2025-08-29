## Data and code for the manuscript "Deconstructing the gender gap in chess ratings" (in prep.)

Persistent gender gaps in achievement are well-documented across competitive and STEM-related fields, yet the underlying drivers remain debated. Chess provides a unique opportunity to study such disparities due to its standardized, quantifiable skill metrics. While previous literature attributes the chess gender gap largely to differential participation rates, rigorous empirical evidence has been limited by restrictive parametric assumptions and incomplete controls for confounding variables. Using a comprehensive, global dataset of over 340,000 internationally rated chess players from 99 national federations, we introduce a nonparametric statistical approach to quantify the extent to which the gender gap in ratings, particularly among elite players, is explained by differences in participation rates, age, and playing experience. We find that these structural factors reduce, but do not eliminate, observed gender differences. However, the remaining difference turns out to be due to lower-rated women being relatively overrepresented in the data. When we only consider players rated above 1400-1600 Elo points, many federations exhibit a negligible gender gap or even favor women. Our findings suggest that innate gender differences in playing ability are implausible as an explanation for the chess gender gap, and also emphasize the need to explore broader social, psychological, and cultural influences beyond simple participation differences.

This repository contains the computer code and data to replicate all our results.



### Software dependencies

R (tested with R 4.2.1, 4.2.2, and 4.3.3). Required R packages:

* `tidyverse`: Efficient data manipulation and plotting
* `Rcpp`: Importing and compiling functions written in C++
* `fs`: Cross-platform file system operations
* `patchwork`: Joining multiple plots together
* `exraDistr`: Working with the negative hypergeometric distribution
* `scales`: Various scale functions for aiding visualization
* `ggtext`: Improved text rendering for ggplot2 plots
* `viridis`: Colorblind-friendly color scales
* `kableExtra`: Creating LaTeX-formatted tables
* `shiny`: Creating interactive applications
* `shinyjs`: Utilities for creating interactive applications
* `shinyBS`: More utilities for creating interactive applications
* `DT`: Searchable and sortable tables in interactive applications

Required non-standard hardware: none. However, reproducing the permutation results with sufficiently many permutations (more than a hundred thousand per sample) greatly benefits from having access to a computing cluster.


### Contents of the `app` directory

This directory contains all required files for running our interactive data explorer application locally, on one's own computer. To run it, simply click the "Run App" button in RStudio or execute

`shiny::runApp(appDir = "./app")`

from the console. The latter method assumes that the working directory is set to be the at root of the project. Alternatively, if the `/app` subdirectory is the designated working directory, then

`shiny::runApp()`

suffices.

* `app.R`: An R Shiny application that can be run locally, as long as the other files in the same directory are also present. The application allows one to quickly and conveniently explore the rating data and permutation results, for all assumptions and data filters.
* `age-experience-tab.rds`: Same as `/data/age-experience-tab.csv`, but in compressed `.rds` format. See description below, under "Contents of the `data` directory". The reason the same data are included here as well is to make their interaction with the application self-contained and not having to rely on files in other directories. (The same goes for the other two data files below.
* `null-stats.rds`: Same as `/data/null-stats.csv`, but in compressed `.rds` format. See description below, under "Contents of the `data` directory".
* `rating-data.rds`: Same as `/data/rating-data.csv`, but in compressed `.rds` format. See description below, under "Contents of the `data` directory".
* `documentation.md`: A Markdown file containing the description of the application. It is viewable, in HTML format, from within the application itself.


### Contents of the `code` directory

* `age-experience-figures.R`: Create scatter plots comparing age and experience across genders for particular data filters, together with histograms of federation-wise rating gaps.
* `conceptual-figure.R`: A figure showing hypothetical gender-wise rating distributions, to illustrate various possible sources of the rating difference.
* `download-and-clean.R`: Automatically download and process the FIDE rating data. The final output consists of two files in the `data` directory: `raw-data.rds` (with data for all months between October 2012 and December 2019), and `rating-data.csv` (with data just for December 2019 - however, with the `games` column containing the total number of games played from Oct 2012 to Dec 2019).
* `generate-perm-table-cluster.R`: A script that will create rating permutations of the ratings of women and men, for each federation, and for a given parameterization. Here "parameterization" means 1) whether junior players (those born after 1999) are excluded, 2) whether inactive players are excluded, 3) whether only players rated below a certain rating floor are excluded (the options are 1000, 1400, or 1600), and 4) which metric is to be calculated (mean, median, sd, top1, or top10). The purpose of this script is to act as one that can be submitted to a large computing cluster. It takes two command-line input parameters:
  - `rownum`: an integer between 1 and 60 (inclusive), determining which combination of the above parameters will be implemented.
  - `perms`: a positive integer, setting the number of random permutations of a given federation's ratings to be generated. For example, if `perms` is `100`, then the ratings for women and men of any given federation will be randomly reshuffled 100 times, and the assigned metric (mean, median, sd, top1, or top10) calculated over those permutations. (*Note:* in practice, at least one million permutations are required for all parameterizations to reach reasonable convergence.)
* `generate-perm-table.R`: As `generate-perm-table-cluster.R`, but performing the permutations for all parameterizations all at once. This means that it can conveniently be used locally, without having access to a computing cluster. (*Warning:* running this script on standard hardware is only feasible with no more than 100,000 permutations. Even that could take quite a while to finish.)
* `global-figures.R`: Generate histograms of the global rating distributions broken down by gender, for various data filters.
* `global-stats.R`: Various statistical tests and analyses performed on the global data (i.e., those not broken down by individual federations).
* `junior-status-analysis.R`: A script to quickly explore the number, proportion, and rating distribution of junior players, broken down by gender.
* `knapp-rank-calculation.R`: A script implementing Knapp's negative hypergeometric rank analysis^[M Knapp (2010). Are participation rates sufficient to explain gender differences in chess performance? *Proc. Royal Soc. B: Biol. Sci.* 277, 2269–2270, doi:10.1098/rspb.2009.2257] for all data filters, and for both global and per-federation ratings.
* `knapp-rank-tables.R`: Organize the results from `knapp-rank-calculation.R` into tidy tables.
* `launch.R`: A script that automatically creates jobs on a computing cluster and submits them, using the `slurm` scheduler. Parameters such as the account name and loaded modules might need to be changed to account for the particular computing cluster setup available to the user.
* `per-federation-analysis.R`: For each chess federation, use the permutation results to see whether one can reject the null hypothesis that observed rating differences between women and men are simply due to a difference in participation.
* `permutation-table.cpp`: Two simple C++ functions, implemented this way to glean more speed. The first function, `top10`, simply takes the average of the ten largest entries from a numerical vector. The second function, `permut_tab`, generates the permutations which are at the heart of the scripts `generate-perm-table.R` and `generate-perm-table-cluster.R`.
* `rating-diff.R`: Obtain the raw, participation-corrected, and participation-, experience-, and age-corrected rating gaps. Results obtained for each federation and parameterization. Currently the script creates a plot, summarizing results across federations via box plots for each parameterization. It also creates two table-like figures (one for global, one for per-federation data) showing what fraction of the observed gender gap is explained by the participation rate hypothesis.
* `stats-from-nulls.R`: This script processes the output generated by `generate-perm-table-cluster.R` (which tends to be massive, in case the number of permutations is large), and saves them in a single CSV file.
* `weighted-regression.R`: Predicting the (participation-corrected) rating difference between women and men per federation, using age and experience as the predictors (without interaction effects). Experience is defined here as the number of games played by a player between Oct 2012 and Dec 2019 (which is the extent of the data available from FIDE). The regression is weighted, with the weights being the inverse variances of the permutation results per federation.
* `perm-test-global`: A directory with versions of the scripts acting on the rating data globally; i.e., by disregarding information on federation. Instead, all players are lumped into one pool for analysis.


### Contents of the `data` directory

* `null-stats.csv`: The result from merging the outputs of `code/stats-from-nulls.R` and `/code/perm-test-global/stats-from-nulls-global.R`. This table has the following columns:
  - `juniors`: `TRUE` or `FALSE`, depending on whether players born after 1999 are included (`TRUE`) or excluded (`FALSE`)
  - `inactives`: `TRUE` or `FALSE`, depending on whether inactive players are included (`TRUE`) or excluded (`FALSE`)
  - `floor`: rating floor (either 1000, 1400, or 1600); players rated below this threshold are excluded
  - `metric`: the metric name (mean, median, sd, top1, or top10)
  - `fed`: three-letter federation code; global data are under `ALL`
  - `stat`: what statistic was measured (`obs`: observed difference between women and men in the data; `ptmean`: difference in permutation means; `ptsd`: difference in permutation standard deviations; `ptpval`: the permutation p-value - that is, the fraction of permutation nulls that fell below the observed difference)
  - `value`: the actual value to the metric-statistic combination
* `age-experience-tab.csv`: The data from the weighted regression, generated by `weighted-regression.R`. The columns are:
  - `metric`: mean, median, sd, top1, or top10
  - `juniors`: `TRUE` or `FALSE`, depending on whether players born after 1999 are excluded
  - `inactives`: `TRUE` or `FALSE`, depending on whether inactive players are excluded
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
* `knapp-rank-global.csv`: The output for the global rating distribution from `code/knapp-rank-calculation.R`.
* `knapp-rank-per-fed.csv`: The output for the per-federation rating distributions from `code/knapp-rank-calculation.R`.
* `global-stat-data.csv`: Summary statistics on the global rating distribution, from Mann-Whitney and Kolmogorov-Smirnov tests.


### Contents of the `figures` directory

* `conceptual.pdf`: A conceptual figure illustrating how the same overall rating gap can arise from multiple underlying causes.
* `global-fig.pdf`: Figure 1 in the main text, with histograms of the global rating distributions for various data filters.
* Twelve files with the names `age-exp-[no]juniors-[no]inactives-1[0|4|6]00.pdf`: Figure 2 in the main text and Figures S1-S11 in the Supplement.
* `summary-fig.pdf`: Figure 3 in the main text, showing raw, P-adjusted, and PEA-adjusted per-federation rating distributions side by side.


### Contents of the `literature` directory

Various related articles, both academic (in the sub-directory `academic`) and other (in the sub-directory `other`).

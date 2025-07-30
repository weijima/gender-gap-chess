This application is a supplement to the manuscript "Deconstructing the gender gap in chess ratings". It allows one to quickly and efficiently explore the rating- and participation gap between genders in chess, for various settings. The application interface is split between two sides. On the left there is a panel of settings which allows the user to adjust various data filters and other parameterization options. On the right we have the corresponding data, organized into four shorter segments: a brief analysis of the global rating distribution (i.e., without breaking the data down into individual federations), a figure, a small number of summary statistics to help interpret the figure, and a table. Below is a detailed description of each of these items.



### Settings panel

Here one can adjust the following options:

* **Include inactive players?** -- If "Yes", then even those FIDE-rated chess players who were inactive as of December 2019 are included in the data. If "No", then they are excluded.

* **Include junior players?** -- If "Yes", then players who are under the age of 20 as of December 2019 are included in the data. If "No", they are excluded. Since the ratings of junior player can be uncertain, removing them can be sensible.

* **Rating floor** -- Whether only those players rated at or above a certain rating threshold should be included. The three options are 1000, 1400, and 1600. Since the dataset as a whole only includes players rated above 1000, the first option does not restrict the pool of players.

  Important note to the above three options: to have sufficient statistical power, we only ever consider federations with at least 30-30 players of each gender. This means that stricter data filters will diminish the number of federations. Therefore fewer federations will show up in the plot, statistics, and table with stricter filters than with more permissive ones.

* **Metric** -- What aspect of the rating difference between genders to display in the plot, for each federation. The difference is always calculated as the statistic for men minus the same statistic for women. The options are:
  - Overall mean gap, for showing the difference between the mean ratings;
  - Overall median gap, for showing the difference between the median ratings;
  - Top 10 gap, for showing the difference between the mean ratings of the top 10 players of each gender;
  - Top 1 gap, for showing the difference between the top-rated man and woman;
  - Standard deviation, for showing the difference between the rating standard deviations. (Note that if this option is chosen, then it makes no sense to make any corrections to ratings. Therefore the **Correction to ratings** option, described below, is shaded out.)

* **Correction to ratings** -- Whether to adjust the naively-observed rating gap of each federation. The options are:
  - None. The unmodified raw rating data are used for making the plot and the summary statistics.
  - Participation correction. We subtract, from the observed rating gap, the difference one would statistically expect based on the fact that the number of players of each gender in any federation is not necessarily balanced (in fact, it is always heavily biased towards men).
  - Participation, age & experience correction. We additionally correct for differences in age and experience (measured as the number of games played since January 2012), based on the statistical model described in the original manuscript.

  For "participation correction", the plot will show federations in two colors: red where the adjusted rating gap is significant (meaning we reject the participation rate hypothesis), and gray where it is not. For the other two options we do not have associated p-values or significance, so all federations are in blue and p-values are not reported in the table. This also means that any adjustments to p-values will not make sense, and so the **Adjustment to p-values** and **Significance threshold** options, described below, are shaded out. (The exception is when **Metric** is set to "Standard deviation", in which case its own p-values and adjustment method can be set.)

* **Adjustment to p-values** -- Whether and how to treat p-values, given the fact that every federation constitutes an independent test and thus we could have problems arising from multiple comparisons. By default, the p-values for every federation are obtained by generating one million permutation samples of the ratings, and tallying how many of the gender differences based on these samples are larger than the true observed rating difference. Dividing this by one million leads to a two-sided quantity 0 &le; *p* &le; 1, where values close to 1 indicate a significant advantage for women and values close to 0 a significant advantage for men. We transform this using the standard method of computing 2 &times; min(*p*, 1 - *p*) and using that result as our p-value. With that, the options here are: (i) "None", which reports the p-values as described; (ii) "False discovery rate" which adjusts these p-values using the method of Benjamini and Hochberg[^1]; (iii) "Holm", which adjusts the p-values using Holm's method[^2].

* **Significance threshold** -- The threshold p-value below which results are displayed as significant in the plot and in the summary statistics. It can be adjusted with the slider on a linear scale between 0.001 and 0.1, in steps of 0.001.



### Global statistics

This shows information on the rating- and participation gap between women and men without disaggregating the data into individual federations. The first line shows the number of women and men, after applying the data filters as specified in the settings panel. The second line is the observed gap for the chosen metric. The third line is the expected gender gap based on differences in participation only, plus/minus one standard deviation (as computed from the distribution of our one million permutation samples). The last line is the p-value for the observed gap, plus an assessment of significance at the level specified by the slider in thesettings panel.



### Plot

The plot is always structured in the same way. The x-axis shows the percentage of women, so it is a measure of the participation gap (with values closer to zero indicating larger gaps). The y-axis shows some measure of the rating gap (men minus women), depending on the setup in the settings panel. Each federation is a point in this plot, shown via its three-letter abbreviation. The dashed horizontal line at y = 0 is simply for visual aid, showing the point of no rating gap. If significant and non-significant results are shown, then the former are in red and the latter in gray. This is also indicated by a color legend at the bottom of the plot. If significance and p-values do not make sense for the chosen setup, then all federations are shown in a neutral blue color.



### Summary statistics

These are displayed directly below the plot. They contain information that can in principle be read off the graph, but is more convenient to have in direct numeric format. The first statistic is the arithmetic mean of the rating gaps across all federations in the plot. The second is the number of federations where the rating gap is positive; i.e., the number of federations in the plot which fall above the dashed line. In case p-values and significance are meaningful for the chosen setup, then the number of significant positive results are also shown (i.e., number of federations falling above the dashed line that are in red). The third and last statistic shows the same, but for federations where the rating gap is negative (women are stronger players).



### Table

This follows right below the summary statistics, and gives a tabular summary of the data. Each row corresponds to a unique federation. The five columns are: federation (with three-letter abbreviations), no. of women (the number of FIDE-rated chess players of the given federation who are women), no. of men (number of players who are men), rating gap (difference in the chosen rating statistic between men and women), and p-value (these are all empty when p-values are not applicable, otherwise they contain the unadjusted or adjusted p-values, depending on user choice). The rating gaps are shown up to two, the p-values up to four decimal precision.

The table is:

* Searchable -- using the search bar at the top right of the table, one can filter for corresponding rows only. For example, one can type "LAT" here to show the data just for the federation of Latvia.
* Paginated -- one can move between pages using the numbered buttons at the bottom right. One can also adjust the number of items shown on one page (top left of the table).
* Sortable -- it is also possible to arrange the data in different ways, by clicking on the small arrows next to the column names. This makes it easy to e.g. pinpoint which federation(s) have the most positive or most negative rating gaps, by simply sorting in descending or ascending order of the **rating gap** column.

<br>

<hr>

[^1]: Benjamini, Y., and Hochberg, Y. (1995). Controlling the false discovery rate: a practical and powerful approach to multiple testing. Journal of the Royal Statistical Society Series B, 57, 289–300. doi:10.1111/j.2517-6161.1995.tb02031.x.

[^2]: Holm, S. (1979). A simple sequentially rejective multiple test procedure. Scandinavian Journal of Statistics, 6, 65–70. https://www.jstor.org/stable/4615733.

library(tidyverse)


rating_data <- read_csv("data/rating-data.csv", col_types = "ccciiil") %>%
  filter(!is.na(born))


# First calculate for active players only

# For active juniors, mean ratings by sex:
rating_data %>%
  filter(active) %>%
  filter(born > 1999) %>%
  summarize(n = n(), mean_rating = mean(rating), .by = sex)

# For active juniors, distribution of ratings by sex:
rating_data %>%
  filter(active) %>%
  filter(born > 1999) %>%
  ggplot(aes(x = rating, fill = sex)) +
  geom_density(color = NA, alpha = 0.4) +
  theme_bw()

# Contingency table - 66.3% of active females and only 29.9% of active males are juniors:
rating_data %>%
  filter(active) %>%
  mutate(junior = born > 1999) %>%
  count(sex, junior) %>%
  summarize(prop = n[junior] / sum(n), .by = sex)

# Chi squared test for the same contingency table (sanity check):
rating_data %>%
  filter(active) %>%
  mutate(junior = born > 1999) %>%
  count(sex, junior) %>%
  xtabs(n ~ junior + sex, data = .) %>%
  chisq.test()


# Repeat the same but include inactive players as well (robustness check):

# Mean ratings by sex:
rating_data %>%
  filter(born > 1999) %>%
  summarize(n = n(), mean_rating = mean(rating), .by = sex)

# Distribution of ratings by sex:
rating_data %>%
  filter(born > 1999) %>%
  ggplot(aes(x = rating, fill = sex)) +
  geom_density(color = NA, alpha = 0.4) +
  theme_bw()

# Contingency table - 50.0% of females and only 24.5% of males are juniors:
rating_data %>%
  mutate(junior = born > 1999) %>%
  count(sex, junior) %>%
  summarize(prop = n[junior] / sum(n), .by = sex)

# Chi squared test for the same contingency table (sanity check):
rating_data %>%
  mutate(junior = born > 1999) %>%
  count(sex, junior) %>%
  xtabs(n ~ junior + sex, data = .) %>%
  chisq.test()

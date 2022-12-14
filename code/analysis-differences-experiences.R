library(magrittr)
library(ggplot2)

# changed line 135635 in the original data file to Abrosimov, Nikolay
experience <- data.table::fread("data/experiencedatafromchessbase.csv", 
                                colClasses = c("numeric", "character", 
                                               "character", "character", 
                                               "character", "numeric", 
                                               "character", "numeric"))

names(experience) <- c("fideid", "last_name", "first_name", 
                       "first_rating_list", "last_rating_list", 
                       "number_rating_lists", "sex", "number_games")


fide <- data.table::fread("data/20201006_FIDE_ratings.csv")


data <- experience %>%
  dplyr::select(fideid, number_games) %>%
  dplyr::inner_join(fide) %>%
  dplyr::filter(!(flag == "i"), 
                !(flag == "wi"))


data %>%
  dplyr::group_by(sex) %>%
  dplyr::summarise(mean = mean(number_games), 
                   max = max(number_games), 
                   n_obs = dplyr::n(), 
                   sum = sum(number_games), 
                   sd = sd(number_games), 
                   cor = cor(rating, number_games))

data_top_10 <- data %>%
  dplyr::group_by(country, sex) %>%
  dplyr::arrange(rating) %>%
  dplyr::slice(10)


# Distribution of experience by gender
ggplot() + 
  geom_histogram(data = data %>%
                   dplyr::filter(number_games < 2000, 
                                 sex == "F"), 
                 aes(y = (..count..)/sum(..count..), 
                     x = number_games, 
                     fill = sex),
                 binwidth = 50, alpha = 0.4) + 
  geom_histogram(data = data %>%
                   dplyr::filter(number_games < 2000, 
                                 sex == "M"), 
                 aes(y = (..count..)/sum(..count..), 
                     x = number_games, 
                     fill = sex),
                 binwidth = 50, alpha = 0.4) + 
  facet_grid(sex ~ .) + 
  theme_minimal() + 
  labs(title = "Relative experience by Gender")


# Distribution of experience by gender
ggplot() + 
  geom_histogram(data = data_top_10 %>%
                   dplyr::filter(number_games < 2000, 
                                 sex == "F"), 
                 aes(y = (..count..)/sum(..count..), 
                     x = number_games, 
                     fill = sex),
                 binwidth = 50, alpha = 0.4) + 
  geom_histogram(data = data_top_10 %>%
                   dplyr::filter(number_games < 2000, 
                                 sex == "M"), 
                 aes(y = (..count..)/sum(..count..), 
                     x = number_games, 
                     fill = sex),
                 binwidth = 50, alpha = 0.4) + 
  facet_grid(sex ~ .) + 
  theme_minimal() + 
  labs(title = "Relative experience by Gender")



data_top_10 %>%
  dplyr::filter(number_games == 0)


# Rating and experience
data %>%
  ggplot(aes(x = number_games, y = rating, color = sex)) + 
  geom_point(alpha = 0.3, 
             size = 0.01) + 
  geom_smooth(method='lm') + 
  theme_minimal() + 
  labs(title = "Correlation between rating and experience")
# could be log-shaped instead?
  
lm(formula = rating ~ sex + number_games + sex * number_games, data = data) %>%
  summary()

glm(formula = rating ~ sex + number_games + sex * number_games, 
    family = gaussian(link = "log"), 
    data = data) %>%
  summary()

# experience differences in countries?
data %>%
  dplyr::group_by()

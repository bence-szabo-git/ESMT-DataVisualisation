options(digits=3)
library(tidyverse)
library(lubridate)
library(here)
library(mosaic)
library(infer)
library(ggridges)
library(viridis)

# while it's fine to know about working directories, I suggest 
# you learn to use the package 'here' that makes organising files easy
# https://malco.io/2018/11/05/why-should-i-use-the-here-package/
movies <- read_csv('Data/movies.csv')

glimpse(movies)

movies %>% 
  count(genre,sort=TRUE)

#choose genres that have at least 5 movies
genres_to_choose <- movies %>%
  group_by(genre) %>% 
  summarise(count = n()) %>% 
  filter(count>5) %>% 
  select(genre) %>% 
  pull # get into vector
genres_to_choose

# Confidence Interval (CI) using the formula mean +- t_critical * SE
genres_formula_ci <- movies %>%
  filter(genre %in% genres_to_choose) %>%
  group_by(genre) %>%
  summarise(
    mean_rating = mean(rating),
    sd_rating = sd(rating),
    count = n(),
    t_critical = qt(0.975, count-1),
    se_rating = sd_rating / sqrt(count),
    margin_error = t_critical * se_rating,
    rating_low = mean_rating - margin_error,
    rating_high = mean_rating + margin_error
  ) %>%
  arrange(desc(mean_rating))

# Plot distributions of ratings, faceted by genre

# Confidence Interval (CI) using the formula mean +- t_critical * SE

#visualise CIs for all genres. 


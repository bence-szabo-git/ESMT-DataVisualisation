# ggplot2 Tutorial - Movies Dataset
# Data Visualisation with R and Tableau - MAAI2025
# Following slides 61-67

# Load required libraries
library(tidyverse)
library(scales)

# Load the movies dataset
movies <- read.csv("Data/movies.csv")

# Preview the data
head(movies)
str(movies)

# ========================================
# 1. DRAWING NUMERICAL VALUES AS BARS (Slide 61)
# ========================================

# Basic bar chart showing all movies (this will be very crowded)
ggplot(movies, aes(x = title, y = gross)) +
  geom_col()

# This creates an unreadable chart - we need to flip axes!

# ========================================
# 2. FLIPPING AXES
# ========================================

# Method 1: Map to y-axis directly (preferred)
ggplot(movies, aes(y = title, x = gross)) +
  geom_col()

# Method 2: Use coord_flip() (older approach)
ggplot(movies, aes(x = title, y = gross)) +
  geom_col() +
  coord_flip()

# Both are still unreadable with all movies - let's filter!

# ========================================
# 3. CHOOSING TOP/BOTTOM 20 MOVIES
# ========================================

# Top 20 highest grossing movies
movies %>%
  slice_max(order_by = gross, n = 20) %>%
  ggplot(aes(x = gross, y = title)) +
  geom_col() +
  theme_minimal(base_size = 6) +
  scale_x_continuous(labels = label_dollar())

# Bottom 20 lowest grossing movies  
movies %>%
  slice_min(order_by = gross, n = 20) %>%
  ggplot(aes(x = gross, y = title)) +
  geom_col() +
  theme_minimal(base_size = 6) +
  scale_x_continuous(labels = label_dollar())

# ========================================
# 4. REORDERING WITH fct_reorder()
# ========================================

# Reorder titles by gross earnings for better visualization
movies %>%
  slice_max(order_by = gross, n = 20) %>%
  mutate(title = fct_reorder(title, gross)) %>%
  ggplot(aes(x = gross, y = title)) +
  geom_col() +
  theme_minimal(base_size = 8) +
  scale_x_continuous(labels = label_dollar()) +
  labs(
    title = "Top 20 Highest Grossing Movies",
    x = "Gross Earnings (US$)",
    y = "Movie Title"
  )

# ========================================
# 5. CREATE THE FOLLOWING FROM IMDB DATA
# ========================================

# Recreate the colored bar chart by genre
# First, let's see what genres we have
table(movies$genre)

# Create the genre-colored chart for top movies
movies %>%
  slice_max(order_by = gross, n = 20) %>%
  mutate(title = fct_reorder(title, gross)) %>%
  ggplot(aes(x = gross, y = title, fill=genre)) +
  geom_col() +
  theme_minimal(base_size = 8) +
  scale_x_continuous(labels = label_dollar()) +
  labs(
    title = "Top 20 Highest Grossing Movies",
    x = "Gross Earnings (US$)",
    y = "Movie Title"
  )

# ========================================
# 6. REORDERING BASED ON FREQUENCY
# ========================================

# Count genres by frequency - ascending order
movies %>%
  mutate(genre = fct_infreq(genre)) %>%
  ggplot(aes(y = genre)) +
  geom_bar() +
  theme_minimal(base_size = 6) +
  labs(
    title = "Movie Genres by Frequency (Ascending)",
    x = "Count",
    y = "Genre"
  )

# Count genres by frequency - descending order (most common first)
movies %>%
  mutate(genre = fct_rev(fct_infreq(genre))) %>%
  ggplot(aes(y = genre)) +
  geom_bar() +
  theme_minimal(base_size = 6) +
  labs(
    title = "Movie Genres by Frequency (Descending)", 
    x = "Count",
    y = "Genre"
  )

# ========================================
# 7. REORDERING BASED ON NUMERICAL VALUES
# ========================================

# Calculate average gross by genre and reorder
movies %>%
  group_by(genre) %>%
  summarise(avg_gross = mean(gross, na.rm = TRUE)) %>%
  mutate(genre = fct_reorder(genre, avg_gross)) %>%
  ggplot(aes(x = avg_gross, y = genre)) +
  geom_col(fill = "steelblue") +
  theme_minimal() +
  scale_x_continuous(labels = label_dollar()) +
  labs(
    title = "Average Box Office Gross by Genre",
    x = "Average Gross Earnings (US$)",
    y = "Genre"
  )

# Alternative: Median gross by genre (less affected by outliers)
movies %>%
  group_by(genre) %>%
  summarise(median_gross = median(gross, na.rm = TRUE)) %>%
  mutate(genre = fct_reorder(genre, median_gross)) %>%
  ggplot(aes(x = median_gross, y = genre)) +
  geom_col(fill = "darkgreen") +
  theme_minimal() +
  scale_x_continuous(labels = label_dollar()) +
  labs(
    title = "Median Box Office Gross by Genre",
    x = "Median Gross Earnings (US$)", 
    y = "Genre"
  )

# ========================================
# 8. TIME SERIES WITH FACETING - LINE GRAPHS BY CATEGORY
# ========================================

# Line graphs show trends over time - perfect for time series data
# Faceting allows us to compare trends across different categories

# Total number of movies produced by genre over time
movies_by_year_genre %>%
  ggplot(aes(x = year, y = total_movies)) +
  geom_line(aes(color = genre), size = 1.2) +
  geom_point(aes(color = genre), size = 2) +
  facet_wrap(~ genre, scales = "free_y", ncol = 3) +
  theme_minimal() +
  theme(legend.position = "none") +  # Remove legend since facets show categories
  labs(
    title = "Movie Production Volume by Genre, 2000-2018", 
    subtitle = "Number of movies released per year in each genre",
    x = "Year",
    y = "Number of Movies Released",
    caption = "Source: IMDB Movies Dataset"
  ) +
  theme(
    strip.text = element_text(size = 10, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Alternative with more comprehensive theme
movies_by_year_genre %>%
  ggplot(aes(x = year, y = total_movies)) +
  geom_line(aes(color = genre), size = 1.5) +  # Thicker lines, no points
  facet_wrap(~ genre, scales = "free_y", ncol = 4) +  # 4 columns like housing chart
  theme_gray() +  # Gray background theme to match housing chart
  theme(
    legend.position = "none",  # Remove legend since facets show categories
    strip.text = element_text(size = 11, face = "bold", color = "black"),  # Bold facet labels
    strip.background = element_rect(fill = "gray80", color = "gray60"),  # Gray facet headers
    panel.grid.major = element_line(color = "white", size = 0.8),  # White grid lines
    panel.grid.minor = element_line(color = "white", size = 0.4),  # Subtle minor grid
    panel.background = element_rect(fill = "gray92"),  # Light gray panel background
    axis.text = element_text(size = 9),  # Smaller axis text
    axis.text.x = element_text(angle = 45, hjust = 1),  # Angled year labels
    plot.background = element_rect(fill = "white"),  # White plot background
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11, color = "gray30")
  ) +
  labs(
    title = "Movie Production Volume by Genre, 2000-2018",
    subtitle = "Number of movies released per year in each genre",
    x = "Year",
    y = "Number of Movies Released",
    caption = "Source: IMDB Movies Dataset"
  )

# ========================================
# ADDITIONAL CHALLENGES
# ========================================

# Explore budget vs gross earnings
ggplot(movies,
       aes(x=budget, y=gross, fill = genre)) +
  geom_point(aes(colour = genre
                 #size = bikes_hired
                 ),
             #shape = 23,
             #colour = "black",  # This overrides the aesthetic mapping
             #alpha = .25
  ) +
  facet_wrap(~ genre) +
  geom_smooth(
    method = "lm",
    se = FALSE
  )

# Rating distribution by genre
movies %>%
  group_by(genre) %>%
  summarise(avg_rating = mean(rating, na.rm = TRUE)) %>%
  #slice_max(order_by=genre) %>%
  #mutate(genre = fct_rev(genre)) %>%
  ggplot(aes(x = avg_rating, y = genre)) +
  geom_col(fill = "steelblue") +
  theme_minimal()
  #scale_x_continuous(labels = label_dollar()) +
  #labs(
  #  title = "Average Box Office Gross by Genre",
  #  x = "Average Gross Earnings (US$)",
  #  y = "Genre"
  

# Top directors by total gross earnings

# Seasonal patterns over time


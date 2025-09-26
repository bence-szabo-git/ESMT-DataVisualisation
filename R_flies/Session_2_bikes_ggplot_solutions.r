# ggplot2 Tutorial - Bikes Dataset
# Data Visualisation with R and Tableau - MAAI2025
# Following slides 40-56

# Load required libraries
library(tidyverse)
library(ggthemes)

# Load the bikes dataset
bikes <- read.csv("Data/bikes_clean.csv")

# Preview the data
head(bikes)
str(bikes)

# ========================================
# 1. AESTHETIC MAPPING
# ========================================

# Basic aesthetic mapping - outside as component
ggplot(data = bikes) +
  aes(x = mean_temp, y = bikes_hired)

# Different ways to specify aesthetics:

# Inside, explicit matching
ggplot(data = bikes, 
       mapping = aes(x = mean_temp, y = bikes_hired))

# Inside, implicit matching
ggplot(bikes, 
       aes(mean_temp, bikes_hired))

# Inside, mixed matching
ggplot(bikes, 
       aes(y = bikes_hired, x = mean_temp))

# ========================================
# 2. GEOMETRIES
# ========================================

# Add points geometry
ggplot(bikes, 
       aes(x = mean_temp, y = bikes_hired)) +
  geom_point()

# ========================================
# 3. VISUAL PROPERTIES OF LAYERS 
# ========================================

# Customize point appearance
ggplot(bikes,
       aes(x = mean_temp, y = bikes_hired)) +
  geom_point(
    colour = "#5989e3",  # colour as a HEX code
    alpha = .25,
    shape = "X",
    stroke = 1,
    size = 4
  )

# ========================================
# 4. SETTING VS MAPPING
# ========================================

# Setting a constant colour
ggplot(bikes,
       aes(x = mean_temp, y = bikes_hired)) +
  geom_point(
    colour = "#5989e3",
    alpha = .25
  )

# Mapping colour to a variable
ggplot(bikes,
       aes(x = mean_temp, y = bikes_hired)) +
  geom_point(
    aes(colour = season_name),
    alpha = .25
  )

# ========================================
# 5. MAPPING EXPRESSIONS 
# ========================================

# Map colour to a logical expression
ggplot(bikes,
       aes(x = mean_temp, y = bikes_hired)) +
  geom_point(
    aes(colour = mean_temp > 20),
    alpha = .25
  )

# ========================================
# 6. SETTING A CONSTANT PROPERTY
# ========================================

# Complex example with multiple aesthetics
ggplot(bikes,
       aes(x = date, y = bikes_hired)) +
  geom_point(
    aes(colour = precipitation > 0,
        size = bikes_hired),
    shape = 23,
    colour = "black",  # This overrides the aesthetic mapping
    alpha = .25
  )

# ========================================
# 7. LOCAL VS GLOBAL ENCODING
# ========================================

# Local encoding (aesthetic in geom layer)
ggplot(bikes,
       aes(x = mean_temp, y = bikes_hired)) +
  geom_point(
    aes(colour = season_name),
    alpha = .25
  )

# Global encoding (aesthetic in ggplot call)
ggplot(bikes,
       aes(x = mean_temp, y = bikes_hired,
           colour = season_name)) +
  geom_point(
    alpha = .25
  )

# ========================================
# 8. GLOBAL COLOUR AESTHETIC
# ========================================

# Global colour affects multiple geoms
ggplot(bikes,
       aes(x = mean_temp, y = bikes_hired,
           colour = season_name)) +
  geom_point(
    alpha = .25
  ) +
  geom_smooth(
    method = "lm",
    se = FALSE
  )

# ========================================
# 9. OVERWRITE GLOBAL AESTHETICS
# ========================================

# Override global colour in one layer
ggplot(bikes,
       aes(x = mean_temp, y = bikes_hired,
           colour = season_name,
           group = season_name)) +
  geom_point(
    alpha = .25
  ) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    colour = "black"  # Overrides the global colour
  )

# ========================================
# 10. STORE GGPLOT AS OBJECT
# ========================================

# Create a base graph object
base_graph <-
  ggplot(bikes,
         aes(x = mean_temp, y = bikes_hired,
             colour = season_name,
             group = season_name)) +
  geom_point(
    alpha = .25
  ) +
  geom_smooth(
    method = "lm",
    se = FALSE
  )

# Check the class
class(base_graph)

# Display the graph
base_graph

# ========================================
# 11. EXTEND GGPLOT OBJECT: ADD LABELS
# ========================================

# Add comprehensive labels
base_graph +
  labs(
    title = "TfL bike sharing trends",
    subtitle = "Reported bike rents versus temperature in London",
    x = "Mean temperature (°C)",
    y = "Total bike rentals",
    caption = "Data: TfL",
    colour = "Season:"
  )

# ========================================
# 12. EXTEND GGPLOT OBJECT: THEMES
# ========================================

# Apply different themes
base_graph + theme_light()

base_graph + theme_minimal()

# ========================================
# 13. USE PRE-DEFINED THEMES FROM {ggthemes}
# ========================================

# Uncomment these lines if you have ggthemes installed
# library(ggthemes)
base_graph + theme_economist()
base_graph + theme_solarized()

# ========================================
# 14. SET A THEME GLOBALLY
# ========================================

# Set a global theme for all subsequent plots
theme_set(theme_light(base_size = 18))

# Now base_graph will use the global theme
base_graph

# Reset to default theme (optional)
theme_set(theme_gray())

base_graph

# ========================================
# 15. HISTOGRAMS
# ========================================

ggplot(bikes, aes(x = bikes_hired)) +
  geom_histogram() +
  theme_bw()

ggplot(bikes, aes(x = bikes_hired, fill = season_name)) +
  geom_histogram() +
  facet_wrap(~ season_name) +
  theme_bw() +
  labs()

ggplot(bikes, aes(x = bikes_hired)) +
  geom_histogram() +
  facet_wrap(~ month_name) +
  theme_bw()

ggplot(bikes, aes(x = bikes_hired)) +
  geom_histogram() +
  facet_wrap(~ month_name, nrow = 4) +
  theme_bw()

# ========================================
# 16. FACETING WITH facet_wrap()
# ========================================

# Density plots show the distribution (shape) of a continuous variable
# Unlike histograms with discrete bars, density plots create smooth curves
# The area under each curve represents the relative frequency/probability

# Example 1: Distribution of bike rentals by season using density plots
# Shows how bike rental frequencies differ across seasons
ggplot(bikes, aes(x = bikes_hired)) +
  geom_density(aes(fill = season_name), alpha = 0.3) +  # alpha controls transparency
  # facet_wrap creates separate panels for each season
  # nrow = 4 arranges panels in 4 rows (vertical stack)
  facet_wrap(~ season_name, nrow = 4) +
  theme_bw() +  # Black and white theme for cleaner appearance
  labs(
    title = "Distribution of Daily Bike Rentals by Season",
    subtitle = "Density curves show the frequency distribution of rental volumes",
    x = "Number of Bikes Hired per Day",
    y = "Density",
    fill = "Season"
  )

# Example 2: Distribution of bike rentals across all 12 months
# Demonstrates faceting with more categories (12 months vs 4 seasons)
ggplot(bikes, aes(x = bikes_hired)) +
  geom_density(aes(fill = season_name), alpha = 0.3) +
  # facet_wrap by month_name creates 12 panels (one per month)
  facet_wrap(~ month_name, nrow = 4) +
  theme_bw() +
  theme(legend.position = "none") +  # Remove legend since colors are redundant with facets
  labs(
    title = "Distribution of Daily Bike Rentals by Month",
    subtitle = "Monthly patterns show seasonal variation in rental volume distributions",
    x = "Number of Bikes Hired per Day", 
    y = "Density"
  )

# Example 3: Separate plots for each season
# Shows temperature vs bike rentals with individual panels for each season
ggplot(bikes, aes(x = mean_temp, y = bikes_hired)) +
  geom_point(alpha = 0.5, color = "steelblue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  facet_wrap(~season_name) +
  theme_minimal() +
  labs(
    title = "Temperature vs Bike Rentals by Season",
    subtitle = "Each season shows different temperature ranges and relationships",
    x = "Mean Temperature (°C)",
    y = "Bikes Hired",
    caption = "Data: TfL"
  )

# ========================================
# ADDITIONAL EXPLORATIONS
# ========================================

# Explore other relationships in the bikes data
ggplot(bikes, aes(x = humidity, y = bikes_hired)) +
  geom_point(aes(colour = weekend), alpha = 0.6) +
  geom_smooth(method = "lm") +
  theme_minimal() +
  labs(
    title = "Bike Rentals vs Humidity",
    x = "Humidity",
    y = "Bikes Hired",
    colour = "Weekend"
  )

# Seasonal patterns over time
ggplot(bikes, aes(x = as.Date(date), y = bikes_hired)) +
  geom_line(aes(colour = season_name), alpha = 0.7) +
  theme_minimal() +
  labs(
    title = "Bike Rentals Over Time by Season",
    x = "Date",
    y = "Bikes Hired",
    colour = "Season"
  )

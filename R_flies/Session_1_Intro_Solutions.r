# ============================================================================
# Brief Introduction to R and Data Visualization
# ============================================================================

# PART 1: QUICK R BASICS
# ============================================================================

# R as a calculator
2 + 3
10 * 5
sqrt(16)

# Variables and assignment
x <- 10
y <- 5
result <- x + y
print(result)

# Vectors (collections of values)
numbers <- c(1, 2, 3, 4, 5)
names <- c("Alice", "Bob", "Charlie")
print(numbers)
print(names)

# Basic functions
length(numbers)
mean(numbers)
max(numbers)

# Data types
class(numbers)    # numeric
class(names)      # character
class(TRUE)       # logical

# Quick peek at data frames (like Excel tables)
simple_df <- data.frame(
  name = c("Alice", "Bob", "Charlie"),
  age = c(25, 30, 35),
  height = c(165, 180, 175)
)
print(simple_df)

# PART 2: LOADING LIBRARIES
# ============================================================================

# Install packages (run once - commented out for class)
# install.packages(c("gapminder", "dplyr", "ggplot2", "mosaic"))

# Load essential libraries for data visualization
library(gapminder)  # Contains our dataset
library(dplyr)      # Data manipulation
library(ggplot2)    # Data visualization
library(mosaic)     # For favstats function

# Check what we loaded
print("Libraries loaded successfully!")

# PART 3: EXPLORING THE GAPMINDER DATASET
# ============================================================================

# What is gapminder?
# It contains data about countries' life expectancy, population, and GDP
# from 1952 to 2007, collected by Hans Rosling's Gapminder Foundation

# First look at the data
head(gapminder)           # First 6 rows
tail(gapminder)           # Last 6 rows
glimpse(gapminder)        # Structure overview

# Basic information about the dataset
nrow(gapminder)           # Number of rows
ncol(gapminder)           # Number of columns  
names(gapminder)          # Column names
dim(gapminder)            # Dimensions (rows, columns)

# Summary statistics
# Basic summary for all variables
summary(gapminder)

# Using favstats for detailed statistics on specific variables
# Life expectancy statistics
favstats(~lifeExp, data = gapminder)

# GDP per capita statistics
favstats(~gdpPercap, data = gapminder)

# Population statistics
favstats(~pop, data = gapminder)

# Statistics by group (e.g., by continent)
favstats(lifeExp ~ continent, data = gapminder)

# Explore the variables
unique(gapminder$continent)    # What continents?
length(unique(gapminder$country))  # How many countries?
range(gapminder$year)          # What years?

# Quick data exploration with dplyr
gapminder %>% 
  filter(year == 2007) %>%     # Most recent year
  arrange(desc(lifeExp)) %>%   # Sort by life expectancy
  head(10)                     # Top 10 countries

# Countries with lowest life expectancy in 2007
gapminder %>% 
  filter(year == 2007) %>% 
  arrange(lifeExp) %>% 
  head(5)

# Average life expectancy by continent in 2007
gapminder %>% 
  filter(year == 2007) %>% 
  group_by(continent) %>% 
  summarise(
    avg_life_exp = mean(lifeExp),
    countries = n()
  )

# BONUS: Quick visualization preview
# ============================================================================

# Simple scatter plot: GDP vs Life Expectancy
ggplot(data = filter(gapminder, year == 2007), 
       aes(x = gdpPercap, y = lifeExp)) +
  geom_point() +
  labs(title = "GDP per Capita vs Life Expectancy (2007)",
       x = "GDP per Capita ($)",
       y = "Life Expectancy (years)")

# With colors by continent
ggplot(data = filter(gapminder, year == 2007), 
       aes(x = gdpPercap, y = lifeExp, color = continent)) +
  geom_point(size = 3, alpha = 0.7) +
  labs(title = "GDP per Capita vs Life Expectancy by Continent (2007)",
       x = "GDP per Capita ($)",
       y = "Life Expectancy (years)",
       color = "Continent")

# ============================================================================
# NEXT STEPS FOR YOUR DATA VISUALIZATION COURSE:
# ============================================================================

# Now you're ready to:
# 1. Create more complex visualizations with ggplot2
# 2. Explore data manipulation with dplyr
# 3. Make interactive plots
# 4. Build dashboards
# 5. Analyze trends over time

print("Welcome to the world of data visualization with R!")
print("The gapminder dataset is loaded and ready for exploration!")
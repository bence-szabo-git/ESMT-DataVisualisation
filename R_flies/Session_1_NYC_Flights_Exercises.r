# ============================================================================
# dplyr Data Manipulation Exercises with NYC Flights Dataset
# Questions Only
# ============================================================================

# SETUP: Load required libraries and data
# ============================================================================

# Install packages if needed (run once)
# install.packages(c("nycflights13", "dplyr", "ggplot2"))

# Load libraries
library(nycflights13)  # Contains flights dataset
library(dplyr)         # Data manipulation
library(ggplot2)       # For visualization

# Load the flights dataset
data(flights)

# Quick look at the data
glimpse(flights)
head(flights)

# ============================================================================
# WARM-UP EXERCISES: Basic dplyr functions
# ============================================================================

# Exercise 1: Filter flights that departed from JFK airport
# Your code here:


# Exercise 2: Select only the columns: year, month, day, carrier, flight, origin, dest
# Your code here:


# Exercise 3: Arrange flights by departure delay (dep_delay) in descending order
# Your code here:


# Exercise 4: Create a new column called "total_delay" that adds dep_delay and arr_delay
# Your code here:


# ============================================================================
# INTERMEDIATE EXERCISES: Combining functions
# ============================================================================

# Exercise 5: Find flights from JFK to LAX that were delayed by more than 2 hours
# (Hint: departure delay > 120 minutes)
# Your code here:


# Exercise 6: For flights in June, select carrier, flight number, origin, destination, 
# and departure delay. Arrange by departure delay (highest first)
# Your code here:


# Exercise 7: Create a new column for flight speed (distance/air_time * 60 for mph)
# Filter for flights with speed > 500 mph, arrange by speed (fastest first)
# Your code here:


# Exercise 8: Find the longest flight distance for each origin airport
# Your code here:


# ============================================================================
# ADVANCED EXERCISES: Complex operations
# ============================================================================

# Exercise 9: Calculate average departure and arrival delays by carrier
# Remove missing values and arrange by average departure delay
# Your code here:


# Exercise 10: Find the most popular destinations (top 10) from each NYC airport
# Show origin, destination, and number of flights
# Your code here:


# Exercise 11: Calculate monthly on-time performance by carrier
# Define on-time as arrival delay <= 0 minutes
# Show carrier, month, total flights, on-time flights, and on-time percentage
# Your code here:


# Exercise 12: Find flights that had the worst delays for each day of the year
# Show the date, carrier, flight number, origin, destination, and departure delay
# Your code here:


# ============================================================================
# CHALLENGE EXERCISES: Complex analysis
# ============================================================================

# Exercise 13: Create a "delay category" column with these categories:
# "Early" (arrival delay < -15), "On Time" (-15 to 15), "Delayed" (15 to 60), "Very Delayed" (> 60)
# Calculate the percentage distribution of each category by carrier
# Your code here:


# Exercise 14: Find the "rush hours" - which hour of the day has the most departures?
# Also calculate average departure delay for each hour
# Your code here:


# ============================================================================
# SUMMARY
# ============================================================================

print("Key functions to practice:")
print("- filter(): Select rows based on conditions")
print("- select(): Choose specific columns") 
print("- arrange(): Sort data")
print("- mutate(): Create new columns")
print("- group_by(): Group data for analysis")
print("- summarise(): Calculate summary statistics")
print("- case_when(): Create conditional categories")
print("- slice_max(): Get top n rows per group")
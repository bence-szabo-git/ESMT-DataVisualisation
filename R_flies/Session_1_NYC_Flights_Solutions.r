# ============================================================================
# dplyr Data Manipulation Exercises with NYC Flights Dataset
# Solutions Included
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

# SOLUTION:
flights %>% 
  filter(origin == "JFK")

# Exercise 2: Select only the columns: year, month, day, carrier, flight, origin, dest
# Your code here:

# SOLUTION:
flights %>% 
  select(year, month, day, carrier, flight, origin, dest)

# Exercise 3: Arrange flights by departure delay (dep_delay) in descending order
# Your code here:

# SOLUTION:
flights %>% 
  arrange(desc(dep_delay))

# Exercise 4: Create a new column called "total_delay" that adds dep_delay and arr_delay
# Your code here:

# SOLUTION:
flights %>% 
  mutate(total_delay = dep_delay + arr_delay)

# ============================================================================
# INTERMEDIATE EXERCISES: Combining functions
# ============================================================================

# Exercise 5: Find flights from JFK to LAX that were delayed by more than 2 hours
# (Hint: departure delay > 120 minutes)
# Your code here:

# SOLUTION:
flights %>% 
  filter(origin == "JFK", dest == "LAX", dep_delay > 120)

# Exercise 6: For flights in June, select carrier, flight number, origin, destination, 
# and departure delay. Arrange by departure delay (highest first)
# Your code here:

# SOLUTION:
flights %>% 
  filter(month == 6) %>% 
  select(carrier, flight, origin, dest, dep_delay) %>% 
  arrange(desc(dep_delay))

# Exercise 7: Create a new column for flight speed (distance/air_time * 60 for mph)
# Filter for flights with speed > 500 mph, arrange by speed (fastest first)
# Your code here:

# SOLUTION:
flights %>% 
  mutate(speed_mph = distance / air_time * 60) %>% 
  filter(speed_mph > 500) %>% 
  arrange(desc(speed_mph))

# Exercise 8: Find the longest flight distance for each origin airport
# Your code here:

# SOLUTION:
flights %>% 
  group_by(origin) %>% 
  summarise(max_distance = max(distance, na.rm = TRUE))

# ============================================================================
# ADVANCED EXERCISES: Complex operations
# ============================================================================

# Exercise 9: Calculate average departure and arrival delays by carrier
# Remove missing values and arrange by average departure delay
# Your code here:

# SOLUTION:
flights %>% 
  group_by(carrier) %>% 
  summarise(
    avg_dep_delay = mean(dep_delay, na.rm = TRUE),
    avg_arr_delay = mean(arr_delay, na.rm = TRUE),
    flight_count = n()
  ) %>% 
  arrange(avg_dep_delay)

# Exercise 10: Find the most popular destinations (top 10) from each NYC airport
# Show origin, destination, and number of flights
# Your code here:

# SOLUTION:
flights %>% 
  group_by(origin, dest) %>% 
  summarise(flight_count = n(), .groups = "drop") %>% 
  group_by(origin) %>% 
  slice_max(flight_count, n = 10) %>% 
  arrange(origin, desc(flight_count))

# Exercise 11: Calculate monthly on-time performance by carrier
# Define on-time as arrival delay <= 0 minutes
# Show carrier, month, total flights, on-time flights, and on-time percentage
# Your code here:

# SOLUTION:
flights %>% 
  filter(!is.na(arr_delay)) %>% 
  group_by(carrier, month) %>% 
  summarise(
    total_flights = n(),
    on_time_flights = sum(arr_delay <= 0),
    on_time_percent = round(on_time_flights / total_flights * 100, 1),
    .groups = "drop"
  ) %>% 
  arrange(carrier, month)

# Exercise 12: Find flights that had the worst delays for each day of the year
# Show the date, carrier, flight number, origin, destination, and departure delay
# Your code here:

# SOLUTION:
flights %>% 
  filter(!is.na(dep_delay)) %>% 
  group_by(year, month, day) %>% 
  slice_max(dep_delay, n = 1) %>% 
  select(year, month, day, carrier, flight, origin, dest, dep_delay) %>% 
  arrange(year, month, day)

# ============================================================================
# CHALLENGE EXERCISES: Complex analysis
# ============================================================================

# Exercise 13: Create a "delay category" column with these categories:
# "Early" (arrival delay < -15), "On Time" (-15 to 15), "Delayed" (15 to 60), "Very Delayed" (> 60)
# Calculate the percentage distribution of each category by carrier
# Your code here:

# SOLUTION:
flights %>% 
  filter(!is.na(arr_delay)) %>% 
  mutate(
    delay_category = case_when(
      arr_delay < -15 ~ "Early",
      arr_delay <= 15 ~ "On Time", 
      arr_delay <= 60 ~ "Delayed",
      TRUE ~ "Very Delayed"
    )
  ) %>% 
  group_by(carrier, delay_category) %>% 
  summarise(count = n(), .groups = "drop") %>% 
  group_by(carrier) %>% 
  mutate(
    total = sum(count),
    percentage = round(count / total * 100, 1)
  ) %>% 
  select(carrier, delay_category, count, percentage) %>% 
  arrange(carrier, delay_category)

# Exercise 14: Find the "rush hours" - which hour of the day has the most departures?
# Also calculate average departure delay for each hour
# Your code here:

# SOLUTION:
flights %>% 
  filter(!is.na(dep_time)) %>% 
  group_by(hour) %>% 
  summarise(
    departures = n(),
    avg_delay = round(mean(dep_delay, na.rm = TRUE), 1)
  ) %>% 
  arrange(desc(departures))

# ============================================================================
# SUMMARY
# ============================================================================

print("Congratulations! You've completed the dplyr exercises.")
print("Key functions practiced:")
print("- filter(): Select rows based on conditions")
print("- select(): Choose specific columns") 
print("- arrange(): Sort data")
print("- mutate(): Create new columns")
print("- group_by(): Group data for analysis")
print("- summarise(): Calculate summary statistics")
print("- case_when(): Create conditional categories")
print("- slice_max(): Get top n rows per group")
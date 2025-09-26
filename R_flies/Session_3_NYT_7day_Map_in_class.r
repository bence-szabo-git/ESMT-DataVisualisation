# ============================================================================
# COVID-19 Seven-Day New Cases Per 100k - Classroom Live Coding Script
# Real-time analysis of COVID-19 trends using rolling averages
# ============================================================================

# SECTION 1: PACKAGE LOADING AND CONFIGURATION
# ============================================================================

# Core data manipulation and visualization ecosystem
library(tidyverse)    # Umbrella package including:
#   - dplyr: data manipulation (filter, mutate, group_by, etc.)
#   - ggplot2: data visualization grammar of graphics  
#   - readr: fast CSV reading with better type parsing
#   - stringr: string manipulation functions
#   - tidyr: data reshaping (pivot, separate, etc.)

# Spatial data handling and geographic analysis
library(sf)           # Simple Features: modern standard for spatial vector data
#   - Replaces older sp package with cleaner syntax
#   - Better integration with tidyverse workflows
#   - More efficient for large spatial datasets

# US Census geographic data and demographic information  
library(tigris)       # Interface to Census TIGER/Line shapefiles
#   - Download county, state, tract boundaries
#   - Includes shift_geometry() for standard US map layout
#   - Automatically handles coordinate reference systems

library(tidycensus)   # Interface to Census American Community Survey (ACS) data
#   - Download population, income, demographic data
#   - Integrates with Census API for up-to-date information
#   - Can return data with or without geometries

# Time series analysis for epidemiological data
library(slider)       # Efficient sliding window calculations  
#   - Rolling sums, means, other aggregations
#   - More efficient than zoo or TTR packages
#   - Designed for time series epidemiological analysis

# Configure package behavior for optimal performance and consistency
options(
  tigris_use_cache = TRUE,    # Cache downloaded boundary files locally
  #   - Avoids re-downloading same files
  #   - Significantly faster for repeated analysis
  #   - Files stored in user's cache directory
  tigris_class = "sf"         # Return spatial data as sf objects (not sp)
  #   - Ensures compatibility with modern spatial workflow
  #   - Better performance and easier syntax
)

# SECTION 2: COVID CASE DATA IMPORT AND PREPROCESSING
# ============================================================================

# Import New York Times COVID-19 county-level data
# Using 'recent' file (last ~month) for faster processing vs. full historical data
nyt <- readr::read_csv("Data/us-counties-recent.csv",
                       # Explicit column type specification for reliable parsing:
                       col_types = "Dcccci")  # D=Date, c=character, i=integer
# Column structure: date, county, state, fips, cases, deaths
# Explicit types prevent parsing issues with mixed data

# Data cleaning and standardization pipeline
nyt <- nyt %>%
  mutate(
    # Standardize FIPS codes to 5-character format with leading zeros
    # FIPS codes are sometimes stored as integers, losing leading zeros
    fips = stringr::str_pad(as.character(fips), 5, pad = "0"),
    # Example: 1001 becomes "01001" (Alabama, Autauga County)
    
    # Convert case and death counts to numeric, handling any parsing issues
    cases  = suppressWarnings(as.numeric(cases)),
    deaths = suppressWarnings(as.numeric(deaths))
    # suppressWarnings() prevents console spam from "Unknown" or "NA" values
    # Non-numeric values become NA, which we handle in the filter below
  ) %>%
  
  # Quality control: remove invalid or incomplete records
  filter(
    !is.na(fips) &           # Remove records without FIPS codes
      nchar(fips) == 5         # Ensure FIPS codes are exactly 5 characters
    #   - Handles cases where padding might fail
    #   - Excludes state-level aggregates (2-3 digits)
    #   - Excludes malformed codes
  )

# SECTION 3: EPIDEMIOLOGICAL CALCULATION - 7-DAY ROLLING AVERAGE
# ============================================================================

# Calculate 7-day rolling new case rates for epidemiological trend analysis
# Rolling averages smooth out day-of-week reporting variations and weekend lags
nyt_rates <- nyt %>%
  
  # Sort data chronologically within each county for time series calculations
  arrange(fips, date) %>%         # Essential for lag() and rolling calculations
  # arrange() ensures data is ordered correctly
  
  # Group operations by county (FIPS code) to calculate within-county trends
  group_by(fips) %>%
  
  # Calculate daily new cases from cumulative totals
  mutate(
    # Compute new cases as difference between consecutive days
    new_cases = pmax(
      cases - dplyr::lag(cases, default = first(cases)),  # Current - previous day
      0                                                   # Minimum of 0 (handle corrections)
    )
    # lag(cases, default = first(cases)): 
    #   - For first date, use first(cases) as "previous" value  
    #   - This assumes cumulative cases start from 0
    # pmax(..., 0):
    #   - Handles data corrections where cumulative count decreases
    #   - Ensures new_cases is never negative
  ) %>%
  
  # Calculate 7-day rolling sum of new cases
  mutate(
    new_cases_7d = slider::slide_dbl(
      new_cases,              # Input vector: daily new cases
      sum,                    # Function to apply: sum over window
      .before = 6,            # Include 6 days before current day
      #   - Total window: today + 6 previous = 7 days
      .complete = TRUE        # Only return values when full 7-day window available
      #   - First 6 days will be NA (incomplete windows)
      #   - Ensures all values represent true 7-day periods
    )
  ) %>%
  
  # Keep only the most recent date for current situation analysis
  filter(date == max(date)) %>%   # Most recent date in dataset for each county
  # Creates "current snapshot" of 7-day trends
  
  # Remove grouping and select essential columns for joining
  ungroup() %>%                   # Remove group_by() for subsequent operations
  select(fips, date, new_cases_7d)  # Keep only necessary columns for efficiency

# SECTION 4: POPULATION DENOMINATOR DATA FROM US CENSUS
# ============================================================================

# Obtain county population estimates for per-capita rate calculations
# Population denominators are essential for meaningful geographic comparisons

# PREREQUISITE: Census API key setup (required for tidycensus)
# Uncomment and run once to set up API key:
# tidycensus::census_api_key("YOUR_KEY", install = TRUE)
# Get free API key at: https://api.census.gov/data/key_signup.html

pop <- get_acs(
  geography = "county",     # County-level data (not state, tract, block group, etc.)
  
  variables = "B01003_001", # ACS variable code for total population
  # B01003_001 = "Total Population" from ACS Subject Tables
  # Alternative: "B25001_001" for housing units
  
  year = 2022,              # Most recent ACS 5-year estimates available
  # ACS 5-year provides most reliable county estimates
  # Released annually with 5-year data collection period
  
  survey = "acs5",          # American Community Survey 5-year estimates
  # More reliable than 1-year estimates for small counties
  # Trade-off: less current but more statistically robust
  
  geometry = FALSE          # Return data only (no spatial boundaries)
  # geometry = FALSE: faster download, smaller memory
  # We'll get boundaries separately from tigris package
) %>% 
  
  # Simplify to essential columns and rename for clarity
  transmute(
    fips = GEOID,           # Census GEOID is same as county FIPS code
    pop = estimate          # Population estimate (margin of error available as 'moe')
  )

# SECTION 5: RATE CALCULATION - CASES PER 100,000 POPULATION
# ============================================================================

# Combine case counts with population data to calculate standardized rates
# Per-capita rates enable meaningful comparison between counties of different sizes
rates <- nyt_rates %>%
  
  # Join COVID cases with population data
  left_join(pop, by = "fips") %>%    # Match on FIPS county codes
  # left_join() keeps all counties from nyt_rates
  # Counties without population data get NA
  
  # Quality control: remove counties without valid population data
  filter(
    !is.na(pop) &           # Remove counties without population estimates
      pop > 0                 # Remove counties with zero or negative population
    #   - Handles data quality issues
    #   - Prevents division by zero errors
  ) %>%
  
  # Calculate standardized rate per 100,000 population
  mutate(
    rate_7d_per_100k = 100000 * new_cases_7d / pop
    # Standard epidemiological rate calculation:
    #   - Multiply by 100,000 for interpretable scale
    #   - Result: "cases per 100,000 residents over 7 days"
    #   - Enables comparison between counties regardless of population size
    #   - Example: 50 cases in 25,000 population = 200 per 100k
  )

# SECTION 6: GEOGRAPHIC BOUNDARIES AND CARTOGRAPHIC PROCESSING
# ============================================================================

# Define territories to exclude from continental US map
# These distant territories would distort the map's bounding box and visual focus
drop_territories <- c(
  "60",  # American Samoa - South Pacific Ocean
  "66",  # Guam - Western Pacific Ocean  
  "69",  # Northern Mariana Islands - Western Pacific Ocean
  "78"   # U.S. Virgin Islands - Caribbean Sea
)
# Note: Puerto Rico (FIPS "72") is NOT excluded, so it appears on the map

# Download and process county boundaries for cartographic display
cnty <- counties(
  cb = TRUE,                # Cartographic boundaries: simplified for mapping
  #   - Faster download and processing
  #   - Adequate detail for choropleth visualization
  #   - Trade-off: less detailed coastlines
  year = 2022               # Match boundary vintage to population data year
  #   - Ensures geographic consistency
  #   - County boundaries can change due to annexations
) %>%
  
  # Geographic filtering: exclude distant territories  
  filter(!STATEFP %in% drop_territories) %>%  # Remove territories from drop list
  # Keeps all 50 states + DC + Puerto Rico
  
  # CRITICAL: Reposition Alaska and Hawaii for standard map layout
  shift_geometry() %>%      # tigris function for cartographic repositioning:
  #   - Scales Alaska to ~35% of actual size
  #   - Moves Alaska to lower-left of continental US
  #   - Repositions Hawaii east of scaled Alaska
  #   - Maintains continental US in normal position
  #   - Creates familiar "textbook" US map appearance
  
  # Apply equal-area projection for accurate visual comparison
  st_transform(5070) %>%    # EPSG:5070 = Albers Conic Equal Area projection
  #   - Preserves area relationships across counties
  #   - Essential for choropleth maps where area = comparison
  #   - Minimizes distortion for continental United States
  #   - Standard choice for US national mapping
  
  # Select and rename columns for clarity and efficiency
  transmute(
    fips = GEOID,           # 5-digit county FIPS code for joining with data
    NAME,                   # County name (for potential labeling/tooltips)  
    STATEFP,                # 2-digit state FIPS code (for potential state analysis)
    geometry                # Spatial polygon geometry for mapping
  )

# SECTION 7: DATA INTEGRATION AND FINAL PREPARATION
# ============================================================================

# Combine geographic boundaries with calculated COVID rates
cnty_data <- cnty %>%
  left_join(rates, by = "fips") %>%           # Match counties with rate data
  # Counties without rate data get NA
  
  # Remove counties without valid rate calculations for clean visualization
  filter(!is.na(rate_7d_per_100k))           # Drop counties with missing rates
# Alternative: keep NAs, color gray

# Calculate precise map extent from processed geographic data
bb <- st_bbox(cnty_data)    # Get bounding box: xmin, ymin, xmax, ymax
# Reflects shifted geometry (Alaska/Hawaii repositioned)
# Ensures map shows all data without excess white space

# SECTION 8: DATA CATEGORIZATION FOR CHOROPLETH VISUALIZATION
# ============================================================================

# Define breakpoints for rate categories based on epidemiological significance
# Categories chosen to distinguish meaningful levels of COVID transmission
breaks <- c(
  0,      # Minimum: counties with 0 new cases per 100k over 7 days
  5,      # Very low transmission (CDC "low" threshold region)
  10,     # Low transmission 
  25,     # Moderate transmission
  50,     # High transmission (CDC "substantial" threshold region)
  100,    # Very high transmission
  200,    # Extreme transmission
  Inf     # Maximum: 200+ cases per 100k over 7 days
)
# Breakpoint considerations:
# - Based on CDC transmission level definitions
# - Roughly logarithmic spacing to capture range variation
# - Round numbers for easy interpretation
# - Sufficient categories to show geographic variation
# - Aligned with public health policy thresholds

# SECTION 9: CHOROPLETH MAP CREATION - TO BE CODED IN CLASS
# ============================================================================

# Predefined color palette for class use
# Sequential orange palette: light to dark representing low to high rates
# ColorBrewer-inspired scheme suitable for epidemiological data
map_colors <- c(
  "#fff5eb",  # Very light cream (0-5 cases)
  "#fee6ce",  # Light orange (5-10 cases)
  "#fdd0a2",  # Medium light orange (10-25 cases)
  "#fdae6b",  # Medium orange (25-50 cases)
  "#fd8d3c",  # Medium dark orange (50-100 cases)
  "#f16913",  # Dark orange (100-200 cases)
  "#d94801",  # Very dark orange (200+ cases)
  "#8c2d04"   # Darkest brown-orange (highest rates)
)

# Custom labels for intuitive interpretation
map_labels <- c(
  "0–5",      # Very low transmission
  "5–10",     # Low transmission
  "10–25",    # Moderate transmission  
  "25–50",    # High transmission
  "50–100",   # Very high transmission
  "100–200",  # Extreme transmission
  "200+"      # Highest transmission (200 to infinity)
)

# TODO: Create the final choropleth visualization using ggplot2
# Use cnty_data as the base dataset
# Add geom_sf() layer with appropriate aesthetics
# Apply the predefined map_colors and map_labels
# Include proper titles and theming

# SECTION 10: TECHNICAL NOTES AND METHODOLOGICAL CONSIDERATIONS
# ============================================================================

# Epidemiological Context:
# - 7-day rolling averages smooth reporting irregularities (weekend effects, etc.)
# - Per-capita rates enable comparison between different population sizes
# - Current snapshot provides real-time situational awareness
# - Categories align with CDC transmission level definitions

# Data Quality Considerations:
# - NYT data aggregated from state/local health departments  
# - Reporting delays and corrections can affect daily new case calculations
# - Population denominators from ACS 2022 (most recent available)
# - Missing counties may indicate data reporting issues or very small populations

# Cartographic Design Principles:
# - Sequential color scheme appropriate for continuous rate data
# - Equal-area projection ensures visual area corresponds to geographic area
# - Clean design focuses attention on data patterns, not map decoration
# - Bounding box optimization eliminates distracting white space

# Performance and Scalability:
# - Using 'recent' NYT data file reduces processing time vs. full historical data
# - Cartographic boundaries balance detail with file size
# - Local caching prevents repeated downloads
# - Efficient sliding window calculations with slider package

# Alternative Analysis Approaches:
# - Different time windows: 3-day, 14-day rolling averages
# - Rate calculations: per capita, per square mile, age-adjusted
# - Geographic levels: state, metropolitan area, ZIP code
# - Temporal analysis: animation over time, trend analysis
# ============================================================================
# COVID-19 Deaths County-Level Choropleth Map - Classroom Live Coding Script
# Professional cartographic visualization of death rates across US counties
# ============================================================================

# SECTION 1: LIBRARY IMPORTS AND INITIAL SETUP
# ============================================================================

# Load essential packages for spatial data analysis and visualization
library(dplyr)   # Data manipulation: filter(), mutate(), select(), join operations
library(sf)      # Simple Features: modern R standard for spatial vector data
library(tigris)  # Interface to US Census TIGER/Line geographic data

# NOTE: You'll also need ggplot2 for visualization (usually auto-loaded with other packages)
# library(ggplot2)  # Uncomment if not automatically available

# Configure tigris package behavior for optimal performance
options(
  tigris_use_cache = TRUE,    # Cache downloaded files locally to avoid re-downloading
  tigris_class = "sf"         # Return spatial data as 'sf' objects (not legacy 'sp' format)
)

# SECTION 2: GEOGRAPHIC SCOPE DEFINITION
# ============================================================================

# Define which US states and territories to include in the analysis
# FIPS (Federal Information Processing Standards) codes uniquely identify geographic areas

# Generate FIPS state codes for all possible US states and territories
# Range 01-56 covers all 50 states + DC + some territories
keep_statefp <- c(
  sprintf("%02d", 1:56)   # Creates: "01", "02", "03", ..., "56"
  # sprintf("%02d", x) ensures 2-digit format with leading zeros
  # Example: 1 becomes "01", 11 becomes "11"
)

# Identify US territories to exclude from the map
# These territories are geographically distant and would distort the map's bounding box
drop <- c(
  "60",  # American Samoa (AS) - Pacific Ocean, south of equator
  "66",  # Guam (GU) - Western Pacific Ocean  
  "69",  # Northern Mariana Islands (MP) - Western Pacific Ocean
  "78"   # U.S. Virgin Islands (VI) - Caribbean Sea
)
# Note: Puerto Rico ("72") is NOT in the drop list, so it WILL be included if present

# Remove excluded territories from our state list
# setdiff(A, B) returns elements in A that are NOT in B
keep_statefp <- setdiff(keep_statefp, drop)

# Final result: keep_statefp contains FIPS codes for:
# - All 50 US states (01-56, excluding dropped territories)
# - District of Columbia (11)  
# - Puerto Rico (72) if present in data

# SECTION 3: DOWNLOAD AND PROCESS COUNTY BOUNDARIES
# ============================================================================

# Download county-level geographic boundaries from US Census Bureau
cnty <- counties(
  cb = TRUE,        # cb = "cartographic boundary": simplified, generalized boundaries
  #   - Smaller file size, faster download/processing
  #   - Less detailed coastlines, adequate for choropleth maps
  #   - Alternative: cb = FALSE gives full-resolution boundaries
  year = 2022       # Use 2022 vintage boundaries (most recent available)
  #   - Boundaries change due to annexations, incorporations, etc.
  #   - Match boundary year to population/demographic data year
) %>%
  
  # Filter counties to only include our desired geographic scope
  filter(STATEFP %in% keep_statefp) %>%   # Keep only counties in states we want
  # STATEFP = 2-digit state FIPS code from county data
  
  # CRITICAL CARTOGRAPHIC OPERATION: Reposition Alaska and Hawaii
  shift_geometry() %>%
  # shift_geometry() performs coordinate transformation:
  #   - Moves Alaska to lower-left of continental US  
  #   - Scales Alaska down (it's huge!) to ~35% of actual size
  #   - Moves Hawaii to lower-left, east of scaled Alaska
  #   - Keeps continental US in its normal position
  #   - This creates the standard "textbook" US map layout
  #   - Without this, Alaska would dominate the map or be cropped out
  
  # Keep only essential data columns and rename for clarity
  transmute(
    fips = GEOID,     # GEOID contains 5-digit county FIPS code (state + county)
    # Example: "06037" = California (06) + Los Angeles County (037)
    geometry          # Spatial geometry column (polygon boundaries)
  ) %>%               # transmute() = select() + mutate(), keeps only specified columns
  
  # Transform to appropriate map projection for US-wide analysis
  st_transform(5070)  # EPSG:5070 = Albers Conic Equal Area projection
# - Preserves area relationships (critical for choropleth maps)
# - Minimizes distortion across continental United States
# - Standard projection for US Census mapping
# - Alternative: 4326 (WGS84) for web maps, 3857 for web mercator

# SECTION 4: DATA INTEGRATION
# ============================================================================

# Combine geographic boundaries with COVID death rate data
# NOTE: This assumes you have a data frame called 'deaths_rate' with:
#       - fips: 5-digit county FIPS code (character, with leading zeros)
#       - deaths_per_100k: COVID deaths per 100,000 population (numeric)

cnty_deaths <- cnty %>% 
  left_join(deaths_rate, by = "fips")
# left_join() keeps ALL counties from cnty, even if no death data available
# Counties without data will have NA values for death rates
# Alternative: inner_join() would drop counties without death data

# Calculate the geographic extent of our processed data
bb <- st_bbox(cnty_deaths)
# st_bbox() returns bounding box coordinates: xmin, ymin, xmax, ymax
# This defines the map's extent and ensures we show all data
# The bbox reflects our shifted geometry (Alaska/Hawaii repositioned)

# SECTION 5: DATA CATEGORIZATION AND CLEANING
# ============================================================================

# Define breakpoints for COVID death rate categories
# These determine color boundaries on the choropleth map
breaks <- c(
  0,      # Minimum value: counties with 0 deaths per 100k
  250,    # Low mortality threshold
  480,    # Medium mortality threshold  
  680,    # High mortality threshold
  Inf     # Maximum value: counties with 680+ deaths per 100k
)
# Breakpoint selection considerations:
# - Based on data distribution (quantiles, natural breaks, etc.)
# - Round numbers for easy interpretation  
# - Sufficient categories to show variation
# - Not so many categories that differences become meaningless

# Remove counties with missing death rate data
cnty_deaths_clean <- cnty_deaths %>%
  filter(!is.na(deaths_per_100k))   # Exclude counties where death rate is NA
# This prevents NA values from appearing as gray/missing on the map
# Alternative approaches:
# - Keep NAs and color them gray: remove this filter, add na.value in scale
# - Impute missing values: replace NAs with 0, mean, or neighboring county average

# SECTION 6: MAP VISUALIZATION - TO BE CODED IN CLASS
# ============================================================================

# Predefined color palette for class use
# Color values: sequential palette from light to dark
# ColorBrewer-inspired oranges/reds suitable for health data
death_colors <- c(
  "#fee8c8",  # Very light orange (0-250 deaths)
  "#fdbb84",  # Light orange (250-480 deaths)  
  "#e34a33",  # Red-orange (480-680 deaths)
  "#b30000"   # Dark red (680+ deaths)
)

# Custom labels for legend (more readable than raw numbers)
death_labels <- c(
  "0–250",      # Lowest category
  "250–480",    # Low-medium category  
  "480–680",    # Medium-high category
  "680+"        # Highest category (680 to infinity)
)

# TODO: Create the choropleth map using ggplot2 grammar of graphics
# Use cnty_deaths_clean as the base dataset
# Add geom_sf() layer with appropriate aesthetics for death rate categories
# Apply the predefined death_colors and death_labels
# Include proper titles, theming, and legend positioning

# SECTION 7: TECHNICAL NOTES AND CONSIDERATIONS
# ============================================================================

# Map Projection Importance:
# - Geographic coordinates (latitude/longitude) are NOT equal-area
# - Visual comparison of areas requires equal-area projection
# - EPSG:5070 (Albers Conic Equal Area) preserves area relationships
# - This ensures that visual area on map corresponds to actual geographic area

# Color Palette Design Principles:
# - Sequential scheme appropriate for continuous data (deaths per capita)
# - Sufficient contrast between adjacent categories for distinction
# - Color-blind friendly (oranges/reds generally accessible)
# - Cultural associations: red = danger/severity appropriate for mortality data

# Data Quality Considerations:
# - County population denominators must be accurate for per-capita calculations
# - COVID death reporting may vary by jurisdiction and time period
# - Missing data handling affects map interpretation
# - FIPS code matching between boundary and data files must be exact

# Performance Optimization:
# - tigris caching reduces download time for repeated analysis
# - Cartographic boundaries (cb = TRUE) balance detail with file size
# - sf format more efficient than legacy sp format for large datasets

# Alternative Approaches:
# - Use state-level data for faster processing: states() instead of counties()
# - Different breakpoint methods: quantile(), classInt::classIntervals()
# - Interactive maps: leaflet, plotly, or tmap packages
# - Different base geometries: Census tracts, ZIP codes, metropolitan areas
# Visualising TS Data

# In this script, we'll perform a visual analysis of a single stock. The ggplot2 package 
# is one of the most popular in R for plotting. Let's first get our imports figured out 
# first, since we have a good number of them now!

# Load required libraries
library(tidyverse)    # For data manipulation and ggplot2
library(lubridate)    # For date handling
library(slider)       # For rolling calculations
library(patchwork)    # For combining plots

# Now we can load the dataset, from a file called `GME_WSB.csv`. This data covers a 
# particularly exciting two-year period for GameStop, a chain of video game retail stores.

# Read and prepare the data
df <- read_csv("Data/GME_WSB.csv") %>%
  mutate(Date = ymd(Date)) %>%
  arrange(Date) %>%
  distinct() %>%
  filter(!is.na(Date))

# View first few rows
head(df)

# Let's create our first simple plot. We'll use the `geom_line()` function to do this.
ggplot(df, aes(x = Date, y = Close)) +
  geom_line() +
  theme_minimal()

# That was easy, but its not very readable, or attractive. Let's try again.

# Styled plot with better formatting
ggplot(df, aes(x = Date, y = Close)) +
  geom_line(aes(color = "Closing Price"), size = 0.8) +
  labs(
    title = "Gamestop in 2020-2022",
    x = "Date",
    y = "USD",
    color = "Legend"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5),
    legend.position = "bottom"
  ) +
  scale_color_manual(values = c("Closing Price" = "#2E86C1"))

# One of the most commonly plotted technical indicators is Bollinger Bands. 
# They consist of two lines:
# - an upper band 2 standard deviations above the 20-day SMA
# - a lower band 2 standard deviations below the 20-day SMA
# 
# When the bands are close, volatility is low. When they are far apart volatility is high. 
# When the price is near the upper band the security may be overbought (ready for a decline), 
# and when the price is near the lower band, the security may be oversold (ready for a jump).

# Calculate Bollinger Bands components
# Note: Assumes 'df' is a data frame with 'Date' and 'Close' columns containing stock price data
df <- df %>%
  
  # STEP 1: Sort data chronologically
  # Critical for time series analysis - calculations must be done in correct order
  arrange(Date) %>%
  
  # STEP 2: Add new columns with rolling calculations
  mutate(
    
    # Simple Moving Average (SMA) - 20-day rolling mean of closing prices
    # .before = 19 means: current row + 19 previous rows = 20-day window
    # .complete = TRUE means: only calculate when full 20 days are available (returns NA otherwise)
    SMA = slider::slide_dbl(Close, mean, .before = 19, .complete = TRUE),
    
    # Standard Deviation (Dev) - 20-day rolling standard deviation of closing prices
    # Measures volatility/spread of prices around the moving average
    # Uses same 20-day window as SMA for consistency
    Dev = slider::slide_dbl(Close, sd, .before = 19, .complete = TRUE),
    
    # Upper Bollinger Band - 2 standard deviations above the moving average
    # Theoretical: ~97.5% of price movements should be below this line
    # Used as resistance level or overbought indicator
    HighBand = SMA + 2 * Dev,
    
    # Lower Bollinger Band - 2 standard deviations below the moving average  
    # Theoretical: ~97.5% of price movements should be above this line
    # Used as support level or oversold indicator
    LowBand = SMA - 2 * Dev
  )

# INTERPRETATION:
# - When price touches HighBand: potentially overbought (consider selling)
# - When price touches LowBand: potentially oversold (consider buying)
# - When bands are wide: high volatility period
# - When bands are narrow: low volatility period (potential breakout coming)
# - Price typically oscillates between the bands ~95% of the time

# BUSINESS APPLICATION:
# - Risk management: Set stop-losses at band levels
# - Entry/exit signals: Buy at lower band, sell at upper band
# - Volatility assessment: Band width indicates market uncertainty
# - Mean reversion strategy: Expect prices to return toward SMA

# Using the above features to calculate the bands and create the plot
ggplot(df, aes(x = Date)) +
  # You can shade the area between the upper and lower bands for emphasis
  geom_ribbon(aes(ymin = LowBand, ymax = HighBand), 
              fill = "red", alpha = 0.1) +
  geom_line(aes(y = Close, color = "Close"), size = 0.8) +
  # You may see the SMA line plotted too
  # geom_line(aes(y = SMA, color = "20-day SMA")) +
  geom_line(aes(y = HighBand, color = "High Band"), 
            linetype = "dashed", size = 0.6) +
  geom_line(aes(y = LowBand, color = "Low Band"), 
            linetype = "dashed", size = 0.6) +
  labs(
    title = "Bollinger Bands",
    x = "Date",
    y = "Price",
    color = "Legend"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5),
    legend.position = "bottom"
  ) +
  scale_color_manual(values = c(
    "Close" = "black",
    "High Band" = "red",
    "Low Band" = "red"
    # "20-day SMA" = "red"
  ))


## Subplots
# There are times when one plot just isn't enough. In this case we can create a subplot, 
# and then plot on its axes. This generally calls for a different approach to ggplot2, 
# so watch carefully!

# Create separate plots that we'll combine
# Adjust the space between the two plots using patchwork

# Plot the close prices on the first (top) subplot  
p3 <- ggplot(df, aes(x = Date, y = Close)) +
  geom_line(size = 0.8) +
  labs(
    title = "Close Price and Volume",
    y = "Close Price"
  ) +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    plot.title = element_text(size = 14, hjust = 0.5)
  )

# Plot the volume on the second (bottom) subplot
p4 <- ggplot(df, aes(x = Date, y = Volume)) +
  geom_col(fill = "grey", alpha = 0.7) +
  labs(
    x = "Date",
    y = "Volume"
  ) +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma_format())

# Combine plots vertically with different heights (3:1 ratio like the original)
combined_plot <- p3 / p4 + 
  plot_layout(heights = c(3, 1)) +
  plot_annotation(theme = theme(plot.margin = margin(10, 10, 10, 10)))

# Display the combined plot
print(combined_plot)

# Alternative approach using gridExtra (commented out)
# library(gridExtra)
# grid.arrange(p3, p4, nrow = 2, heights = c(3, 1))

# Define specific events manually (you can customize these based on your data)
# Example events - adjust dates and descriptions based on your actual data
key_events <- tribble(
  ~Date, ~Event, ~Type,
  as.Date("2021-01-28"), "Reddit Rally Peak", "Peak",
  as.Date("2021-02-02"), "Sharp Decline", "Drop", 
  as.Date("2021-03-10"), "Second Wave", "Rise",
  as.Date("2021-03-24"), "Correction", "Drop"
) %>%
  left_join(df, by = "Date") %>%
  filter(!is.na(Close))

# Enhanced version with custom event annotations
ggplot(df, aes(x = Date)) +
  geom_ribbon(aes(ymin = LowBand, ymax = HighBand), 
              fill = "red", alpha = 0.1) +
  geom_line(aes(y = Close, color = "Close"), size = 0.8) +
  geom_line(aes(y = HighBand, color = "High Band"), 
            linetype = "dashed", size = 0.6) +
  geom_line(aes(y = LowBand, color = "Low Band"), 
            linetype = "dashed", size = 0.6) +
  
  # Custom event annotations
  geom_point(data = key_events,
             aes(y = Close, color = Type), 
             size = 5, alpha = 0.8) +
  
  geom_text(data = key_events,
            aes(y = Close, label = Event, color = Type),
            vjust = -1.2, hjust = 0.5, size = 3.5, fontface = "bold",
            check_overlap = TRUE) +
  
  labs(
    title = "GameStop Bollinger Bands with Key Events",
    subtitle = "Major market events and price movements annotated",
    x = "Date",
    y = "Price ($)",
    color = "Legend"
  ) +
  
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray50"),
    legend.position = "bottom"
  ) +
  
  scale_color_manual(values = c(
    "Close" = "black",
    "High Band" = "red",
    "Low Band" = "red",
    "Peak" = "#FF6B35",     # Orange for peaks
    "Rise" = "#2E8B57",     # Green for rises  
    "Drop" = "#DC143C"      # Red for drops
  )) +
  
  scale_y_continuous(labels = scales::dollar_format())


# Multi Stock Analysis  
# Let's work with some multiple stock panel data now. We'll use the same libraries as 
# before, but we'll also introduce `corrplot` and additional ggplot2 extensions - 
# other increasingly popular visualisation libraries in R. Again, we'll start with 
# all of our imports.

library(tidyverse)    # For data manipulation and ggplot2
library(lubridate)    # For date handling  
library(corrplot)     # For correlation matrix visualization (R equivalent of seaborn heatmap)
library(viridis)      # For color palettes
library(scales)       # For formatting axes
library(RColorBrewer) # Additional color palettes

# Let's load the panel data as normal.
df <- read_csv("Data/top_six_2020_2025.csv") %>%
  mutate(DlyCalDt = dmy(DlyCalDt))  # dmy() for day-first date parsing

head(df)

# Let's say we wanted to visualise the closing prices of all the stocks in our data set. 
# Reshaping our **long** data to **wide** makes plotting *really* straightforward.
# 
# *(Note this follows ggplot2's philosophy of keeping data in long format, but we can 
# also show the wide format approach)*

# Wide format approach (similar to pandas pivot)
pivot_df <- df %>%
  select(DlyCalDt, Ticker, DlyClose) %>%
  pivot_wider(names_from = Ticker, values_from = DlyClose) %>%
  arrange(DlyCalDt)

print(pivot_df)

# Plot using wide format data - convert back to long for ggplot2
pivot_df %>%
  pivot_longer(cols = -DlyCalDt, names_to = "Ticker", values_to = "DlyClose") %>%
  ggplot(aes(x = DlyCalDt, y = DlyClose, color = Ticker)) +
  geom_line(size = 0.8) +
  labs(
    title = 'Closing Prices of All Stocks',
    x = "Date",
    y = 'Price',
    color = "Stock"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5),
    legend.position = "bottom"
  ) +
  scale_y_continuous(labels = dollar_format()) +
  scale_color_brewer(type = "qual", palette = "Set2")

# Alternative: Direct plotting from long format (more idiomatic R/ggplot2)
df %>%
  ggplot(aes(x = DlyCalDt, y = DlyClose, color = Ticker)) +
  geom_line(size = 0.8) +
  labs(
    title = 'Closing Prices of All Stocks (Long Format)',
    x = "Date",
    y = 'Price',
    color = "Stock"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5),
    legend.position = "bottom"
  ) +
  scale_y_continuous(labels = dollar_format()) +
  scale_color_viridis_d()

# To understand the relationship between the daily returns of different stocks, 
# we've previously used a correlation matrix. After finding daily returns and 
# calculating the correlation with `cor()`, we can use `corrplot`'s `corrplot()` 
# or ggplot2 to visualise it.

# Calculate daily returns and correlation matrix
correlation_matrix <- pivot_df %>%
  select(-DlyCalDt) %>%
  mutate(across(everything(), ~ (.x / lag(.x) - 1))) %>%  # pct_change equivalent
  na.omit() %>%
  cor()

print(correlation_matrix)

# Method 1: Using corrplot (most similar to seaborn heatmap)
corrplot(correlation_matrix, 
         method = "color",
         type = "upper",
         order = "hclust", 
         col = colorRampPalette(c("#053061", "#2166AC", "#4393C3", "#92C5DE",
                                  "#D1E5F0", "#FFFFFF", "#FDDBC7", "#F4A582",
                                  "#D6604D", "#B2182B", "#67001F"))(200),
         tl.cex = 0.8,
         tl.col = "black",
         cl.cex = 0.8,
         title = "Correlation Matrix of Daily Returns",
         mar = c(0,0,2,0))

# Method 2: Using ggplot2 (more customizable)
correlation_df <- correlation_matrix %>%
  as.data.frame() %>%
  rownames_to_column("Stock1") %>%
  pivot_longer(cols = -Stock1, names_to = "Stock2", values_to = "Correlation")

ggplot(correlation_df, aes(x = Stock1, y = Stock2, fill = Correlation)) +
  geom_tile(color = "white", size = 0.5) +
  scale_fill_gradient2(low = "#053061", mid = "white", high = "#67001F",
                       midpoint = 0, space = "Lab",
                       name = "Correlation\nCoefficient") +
  labs(
    title = 'Correlation Matrix of Daily Returns',
    x = "Stock",
    y = "Stock"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(angle = 0, hjust = 1),
    panel.grid = element_blank()
  ) +
  coord_fixed() +
  geom_text(aes(label = round(Correlation, 2)), color = "black", size = 3)

# Method 3: Simple correlation plot without annotations (cleaner look)
ggplot(correlation_df, aes(x = Stock1, y = Stock2, fill = Correlation)) +
  geom_tile(color = "white", size = 0.5) +
  scale_fill_viridis_c(name = "Correlation\nCoefficient") +
  labs(
    title = 'Correlation Matrix of Daily Returns (Viridis)',
    x = "Stock",
    y = "Stock"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  ) +
  coord_fixed()

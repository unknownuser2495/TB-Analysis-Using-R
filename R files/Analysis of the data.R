# Load required libraries
# install.packages("tidyverse")
library(tidyverse)
library(ggplot2)
library(scales)

# Read the data (assuming it's in a CSV file named 'tb_data.csv')
tb_data = read.csv("TB.csv")

# Basic data exploration
print(summary(tb_data))

# Check for missing values
print(colSums(is.na(tb_data)))

# Check unique values in country column
print(length(unique(tb_data$country)))


country_counts <- tb_data %>%count(country) %>%arrange(desc(n))

print("Number of entries per country:")
print(country_counts)

# Convert e_inc_100k to numeric if it's not already
tb_data$e_inc_100k <- as.numeric(as.character(tb_data$e_inc_100k))

# Check for any conversion issues
print(sum(is.na(tb_data$e_inc_100k)))   #some issue in this line of code...

# Function to get the most recent year for each country
get_most_recent_year <- function(data) {
  data %>%
    group_by(country) %>%
    filter(year == max(year)) %>%
    ungroup()
}

# Get the most recent data for each country
tb_data_recent <- get_most_recent_year(tb_data)

# Top 10 countries with highest TB incidence rate (using most recent data)
top_10_incidence <- tb_data_recent %>%
  arrange(desc(e_inc_100k)) %>%
  select(country, year, e_inc_100k) %>%
  head(10)

print("Top 10 countries with highest TB incidence rate (most recent year):")
print(top_10_incidence)

# Visualize top 10 countries with highest TB incidence rate
ggplot(top_10_incidence, aes(x = reorder(country, -e_inc_100k), y = e_inc_100k)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  labs(title = "Top 10 Countries with Highest TB Incidence Rate",
       subtitle = "Based on most recent year's data",
       x = "Country",
       y = "Estimated Incidence per 100,000 population") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Check distribution of e_inc_100k (using most recent data)
print(summary(tb_data_recent$e_inc_100k))

# Plot histogram of e_inc_100k (using most recent data)
ggplot(tb_data_recent, aes(x = e_inc_100k)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "black") +
  theme_minimal() +
  labs(title = "Distribution of TB Incidence Rates",
       subtitle = "Based on most recent year's data",
       x = "Estimated Incidence per 100,000 population",
       y = "Count")

# Time trend analysis for top 5 countries
top_5_countries <- top_10_incidence$country[1:5]

time_trend_top5 <- tb_data %>%
  filter(country %in% top_5_countries) %>%
  ggplot(aes(x = year, y = e_inc_100k, color = country)) +
  geom_line() +
  theme_minimal() +
  labs(title = "TB Incidence Trends for Top 5 Countries",
       x = "Year",
       y = "Estimated Incidence per 100,000 population") +
  theme(legend.position = "bottom")

print(time_trend_top5)

# Global TB-HIV co-infection analysis
tb_hiv_summary <- tb_data %>%
  summarise(
    mean_tbhiv_prct = mean(e_tbhiv_prct, na.rm = TRUE),
    median_tbhiv_prct = median(e_tbhiv_prct, na.rm = TRUE),
    min_tbhiv_prct = min(e_tbhiv_prct, na.rm = TRUE),
    max_tbhiv_prct = max(e_tbhiv_prct, na.rm = TRUE)
  )

print(tb_hiv_summary)

# Correlation between TB incidence and mortality
correlation <- cor(tb_data$e_inc_100k, tb_data$e_mort_100k, use = "complete.obs")
print(paste("Correlation between TB incidence and mortality:", round(correlation, 3)))

# Scatter plot of TB incidence vs mortality
ggplot(tb_data, aes(x = e_inc_100k, y = e_mort_100k)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  theme_minimal() +
  labs(title = "TB Incidence vs Mortality",
       x = "Estimated Incidence per 100,000 population",
       y = "Estimated Mortality per 100,000 population")

# Regional analysis
regional_summary <- tb_data %>%
  group_by(g_whoregion) %>%
  summarise(
    mean_incidence = mean(e_inc_100k, na.rm = TRUE),
    mean_mortality = mean(e_mort_100k, na.rm = TRUE),
    mean_tbhiv_prct = mean(e_tbhiv_prct, na.rm = TRUE)
  )

print(regional_summary)

# Visualize regional analysis
ggplot(regional_summary, aes(x = g_whoregion, y = mean_incidence)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  labs(title = "Mean TB Incidence by WHO Region",
       x = "WHO Region",
       y = "Mean Incidence per 100,000 population") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Time trend analysis (assuming 'year' is present in the dataset)
time_trend <- tb_data %>%
  group_by(year) %>%
  summarise(
    mean_incidence = mean(e_inc_100k, na.rm = TRUE),
    mean_mortality = mean(e_mort_100k, na.rm = TRUE)
  )

ggplot(time_trend, aes(x = year)) +
  geom_line(aes(y = mean_incidence, color = "Incidence")) +
  geom_line(aes(y = mean_mortality, color = "Mortality")) +
  theme_minimal() +
  labs(title = "Global TB Incidence and Mortality Trends",
       x = "Year",
       y = "Rate per 100,000 population",
       color = "Metric") +
  scale_color_manual(values = c("Incidence" = "blue", "Mortality" = "red"))

# Case Fatality Ratio (CFR) analysis
cfr_summary <- tb_data %>%
  summarise(
    mean_cfr = mean(cfr, na.rm = TRUE),
    median_cfr = median(cfr, na.rm = TRUE),
    min_cfr = min(cfr, na.rm = TRUE),
    max_cfr = max(cfr, na.rm = TRUE)
  )

print(cfr_summary)

# Histogram of Case Fatality Ratio
ggplot(tb_data, aes(x = cfr)) +
  geom_histogram(binwidth = 0.05, fill = "steelblue", color = "black") +
  theme_minimal() +
  labs(title = "Distribution of Case Fatality Ratio",
       x = "Case Fatality Ratio",
       y = "Count")

# ggsave("top_10_incidence.png", width = 10, height = 6)
# ggsave("incidence_vs_mortality.png", width = 8, height = 6)
# ggsave("regional_incidence.png", width = 10, height = 6)
# ggsave("time_trend.png", width = 10, height = 6)
# ggsave("cfr_distribution.png", width = 8, height = 6)
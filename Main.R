# Load necessary libraries
source('Dependency Files/Library.R', chdir = TRUE)

#Loading the dataset and cleaning them
source('Dependency Files/Data_Cleaning.R', chdir = TRUE)

#General Overview of the data
source("Dependency Files/General_Overview.R", chdir = TRUE)


                          #For TB_burden_age_sex

# Mean, median, etc. for specific columns
mean(TB_burden_age_sex$best, na.rm = TRUE)
median(TB_burden_age_sex$best, na.rm = TRUE)

# Summarize measure by country
measure_by_country <- TB_burden_age_sex %>%
  group_by(country) %>%
  summarize(total_measure = sum(best, na.rm = TRUE))


# Filter the data frame to include only the top 10 countries based on total_measure
top_10_countries <- measure_by_country %>%
  arrange(desc(total_measure)) %>%
  head(10)

# Create a bar plot for the top 10 countries
ggplot(top_10_countries, aes(x = reorder(country, -total_measure), y = total_measure)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Top 10 Countries by Measure", x = "Country", y = "Total Measure")

# Box plot of measure by age group
ggplot(TB_burden_age_sex, aes(x = age_group, y = best)) +
  geom_boxplot() +
  labs(title = "Measure by Age Group", x = "Age Group", y = "Measure")


# Faceted bar plot of measure by sex and age group
ggplot(TB_burden_age_sex, aes(x = age_group, y = best, fill = sex)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ sex) +
  labs(title = "Measure by Sex and Age Group", x = "Age Group", y = "Measure")


# Summarize total measure by country
total_measure_by_country <- TB_burden_age_sex %>%
  group_by(country) %>%
  summarize(total_measure = sum(best, na.rm = TRUE)) %>%
  arrange(desc(total_measure)) %>%
  top_n(10)

# Extract the top 10 countries
top_10_countries <- total_measure_by_country$country

# Filter the data for the top 10 countries
filtered_data <- TB_burden_age_sex %>%
  filter(country %in% top_10_countries)

# Summarize measure by country and year for the top 10 countries
measure_by_country_year <- filtered_data %>%
  group_by(country, year) %>%
  summarize(total_measure = sum(best, na.rm = TRUE))

# Heatmap of measure by country and year
ggplot(measure_by_country_year, aes(x = year, y = country, fill = total_measure)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "Measure by Country and Year", x = "Year", y = "Country", fill = "Total Measure")

# Summarize measure by risk factor
measure_by_risk_factor <- TB_burden_age_sex %>%
  group_by(risk_factor) %>%
  summarize(total_measure = sum(best, na.rm = TRUE))

# Pie chart of measure by risk factor
ggplot(measure_by_risk_factor, aes(x = "", y = total_measure, fill = risk_factor)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() +
  labs(title = "Measure by Risk Factor", fill = "Risk Factor")


# Print column names to identify the correct ones
print(colnames(TB_burden_age_sex))

# Select relevant columns for analysis (adjust based on actual column names in your data)
relevant_columns <- c("iso3", "age_group", "best", "risk_factor")

# Subset the dataset with relevant columns
tb_analysis <- TB_burden_age_sex[, relevant_columns]

# Rename columns for clarity
colnames(tb_analysis)[colnames(tb_analysis) == "best"] <- "incidence_rate"

# Clean the 'risk_factor' column
tb_analysis$risk_factor <- factor(tb_analysis$risk_factor)

# Replace missing values in 'incidence_rate' with the mean within each group (grouped by 'iso3' and 'age_group')
tb_analysis <- tb_analysis %>%
  group_by(iso3, age_group) %>%
  mutate(incidence_rate = ifelse(is.na(incidence_rate), mean(incidence_rate, na.rm = TRUE), incidence_rate)) %>%
  ungroup()

# Check for any remaining missing values in 'incidence_rate' and handle them by replacing with the global mean
global_mean_incidence <- mean(tb_analysis$incidence_rate, na.rm = TRUE)
tb_analysis$incidence_rate[is.na(tb_analysis$incidence_rate)] <- global_mean_incidence

# Summary statistics for incidence rate by country and risk factor
summary_stats_risk <- tb_analysis %>%
  group_by(iso3, risk_factor) %>%
  summarise(mean_incidence = round(mean(incidence_rate, na.rm = TRUE)),
            sd_incidence = round(sd(incidence_rate, na.rm = TRUE)),
            count = n())

# Print the summary statistics by risk factor
print(summary_stats_risk)

# Correlation Analysis (if applicable)
numeric_columns <- tb_analysis %>% select(incidence_rate)
cor_matrix <- cor(numeric_columns, use = "complete.obs")
print("Correlation Matrix:")
print(cor_matrix)

# Hypothesis Testing: One-way ANOVA for incidence rate across different risk factors
anova_result <- aov(incidence_rate ~ risk_factor, data = tb_analysis)
print("ANOVA Result for Incidence Rate by Risk Factor:")
print(summary(anova_result))

# Select top 10 countries based on mean incidence rate
top_countries <- summary_stats_risk %>%
  arrange(desc(mean_incidence)) %>%
  select(iso3) %>%
  distinct() %>%
  head(10) %>%
  pull(iso3)

# Filter the data for these countries
filtered_tb_analysis <- tb_analysis %>% filter(iso3 %in% top_countries)

# Check if filtered data is empty or has fewer than expected countries
if (nrow(filtered_tb_analysis) == 0) {
  stop("No data available for the selected top countries.")
}

# Ensure there are 10 unique countries in the filtered dataset
print(unique(filtered_tb_analysis$iso3))

# Create a scatter plot for incidence rate 
ggplot(filtered_tb_analysis, aes(x = iso3, y = incidence_rate)) +
  geom_point(size = 2, alpha = 0.7, color = "darkviolet") + 
  labs(title = "Incidence Rate by Country for Different Risk Factors (Top 10 Countries)",
       x = "Country (ISO3)",
       y = "Incidence Rate") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~ risk_factor) +  # Separate by risk factor for clarity
  theme(legend.position = "none")  # Remove legend since all points are the same color


            #For TB_expenditure_utilisation


# Summarize total expenditure by country
exp_by_country <- TB_expenditure_utilisation %>%
  group_by(country) %>%
  summarize(total_exp = sum(exp_tot, na.rm = TRUE)) %>%
  arrange(desc(total_exp)) %>%
  top_n(10)

# Bar plot of total expenditure by country for the top 10 countries
ggplot(exp_by_country, aes(x = reorder(country, -total_exp), y = total_exp)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Total Expenditure by Country (Top 10)", x = "Country", y = "Total Expenditure")

# Summarize total expenditure by year
exp_by_year <- TB_expenditure_utilisation %>%
  group_by(year) %>%
  summarize(total_exp = sum(exp_tot, na.rm = TRUE))

# Line plot of total expenditure over time
ggplot(exp_by_year, aes(x = year, y = total_exp)) +
  geom_line() +
  labs(title = "Total Expenditure Over Time", x = "Year", y = "Total Expenditure")


# Scatter plot of expenditure vs. received funds
ggplot(TB_expenditure_utilisation, aes(x = rcvd_tot, y = exp_tot)) +
  geom_point() +
  labs(title = "Expenditure vs. Received Funds", x = "Received Funds", y = "Expenditure")


# Box plot of expenditure by region
ggplot(TB_expenditure_utilisation, aes(x = g_whoregion, y = exp_tot)) +
  geom_boxplot() +
  labs(title = "Expenditure by Region", x = "Region", y = "Expenditure")



# Correlation between expenditure and received funds
cor(TB_expenditure_utilisation$exp_tot, TB_expenditure_utilisation$rcvd_tot, use = "pairwise.complete.obs")

# Summarize TB_expenditure_utilisation by region
region_summary <- TB_expenditure_utilisation %>%
  group_by(g_whoregion) %>%
  summarize(total_exp = sum(exp_cpp_dstb, na.rm = TRUE))

# Create a bar plot of total expenditures by region
ggplot(region_summary, aes(x = reorder(g_whoregion, -total_exp), y = total_exp)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Total Expenditures by Region", x = "Region", y = "Total Expenditure") +
  theme_minimal()


                            #For TB_outcomes_age_sex


outcomes_long <- TB_outcomes_age_sex %>%
  select(country, year, age_group, sex, succ, fail, died, lost, neval) %>%
  pivot_longer(cols = c(succ, fail, died, lost, neval), names_to = "outcome", values_to = "count")

# Create a bar plot of treatment outcomes
ggplot(outcomes_long, aes(x = outcome, y = count, fill = outcome)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ year) +
  labs(title = "Distribution of TB Treatment Outcomes by Year", x = "Outcome", y = "Count") +
  theme_minimal()



# Summarize TB_outcomes_age_sex by age group and sex
demographic_summary <- TB_outcomes_age_sex %>%
  group_by(age_group, sex) %>%
  summarize(total_succ = sum(succ, na.rm = TRUE),
            total_fail = sum(fail, na.rm = TRUE),
            total_died = sum(died, na.rm = TRUE),
            total_lost = sum(lost, na.rm = TRUE),
            total_neval = sum(neval, na.rm = TRUE))

# Create a bar plot of treatment outcomes by age group and sex
ggplot(demographic_summary, aes(x = age_group, y = total_succ, fill = sex)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Successful TB Treatment Outcomes by Age Group and Sex", x = "Age Group", y = "Count") +
  theme_minimal()


# Melt the TB_outcomes_age_sex for easier plotting
outcomes_long <- TB_outcomes_age_sex %>%
  select(year, succ, fail, died, lost, neval) %>%
  pivot_longer(cols = c(succ, fail, died, lost, neval), names_to = "outcome", values_to = "count")

# Create a stacked bar plot of treatment outcomes by year
ggplot(outcomes_long, aes(x = factor(year), y = count, fill = outcome)) +
  geom_bar(stat = "identity") +
  labs(title = "Distribution of TB Treatment Outcomes by Year", x = "Year", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# Select relevant columns for correlation analysis
outcome_columns <- TB_outcomes_age_sex %>%
  select(succ, fail, died, lost, neval)

# Summarize TB_outcomes_age_sex by region
region_summary <- TB_outcomes_age_sex %>%
  group_by(g_whoregion) %>%
  summarize(total_succ = sum(succ, na.rm = TRUE),
            total_fail = sum(fail, na.rm = TRUE),
            total_died = sum(died, na.rm = TRUE),
            total_lost = sum(lost, na.rm = TRUE),
            total_neval = sum(neval, na.rm = TRUE))

# Create a boxplot of total successful treatments by region
ggplot(region_summary, aes(x = g_whoregion, y = total_succ)) +
  geom_boxplot() +
  labs(title = "Total Successful TB Treatments by Region", x = "Region", y = "Total Successful Treatments") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Filter TB_outcomes_age_sex for a specific year
year_TB_outcomes_age_sex <- TB_outcomes_age_sex %>%
  filter(year == 2020) %>%
  select(succ, fail, died, lost, neval) %>%
  pivot_longer(cols = everything(), names_to = "outcome", values_to = "count")

# Create a pie chart of treatment outcomes for the year 2020
ggplot(year_TB_outcomes_age_sex, aes(x = "", y = count, fill = outcome)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Proportion of TB Treatment Outcomes in 2020", fill = "Outcome") +
  theme_void()

# # Calculate the correlation matrix
# cor_matrix <- cor(outcome_columns, use = "pairwise.complete.obs")
# 
# # Create a heatmap of the correlation matrix
# corrplot(cor_matrix, method = "color")


                  #For TB

# Check for missing values
print(colSums(is.na(TB)))

# Check unique values in country column
print(length(unique(TB$country)))

country_counts <- TB %>%count(country) %>%arrange(desc(n))

print("Number of entries per country:")
print(country_counts)

# Convert e_inc_100k to numeric if it's not already
TB$e_inc_100k <- as.numeric(as.character(TB$e_inc_100k))

# Check for any conversion issues
#print(sum(is.na(TB$e_inc_100k)))

# Function to get the most recent year for each country
get_most_recent_year <- function(data) {
  data %>%
    group_by(country) %>%
    filter(year == max(year)) %>%
    ungroup()
}

# Get the most recent data for each country
TB_recent <- get_most_recent_year(TB)

# Top 10 countries with highest TB incidence rate (using most recent data)
top_10_incidence <- TB_recent %>%
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
print(summary(TB_recent$e_inc_100k))

# Plot histogram of e_inc_100k (using most recent data)
ggplot(TB_recent, aes(x = e_inc_100k)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "black") +
  theme_minimal() +
  labs(title = "Distribution of TB Incidence Rates",
       subtitle = "Based on most recent year's data",
       x = "Estimated Incidence per 100,000 population",
       y = "Count")

# Time trend analysis for top 5 countries
top_5_countries <- top_10_incidence$country[1:5]

time_trend_top5 <- TB %>%
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
tb_hiv_summary <- TB %>%
  summarise(
    mean_tbhiv_prct = mean(e_tbhiv_prct, na.rm = TRUE),
    median_tbhiv_prct = median(e_tbhiv_prct, na.rm = TRUE),
    min_tbhiv_prct = min(e_tbhiv_prct, na.rm = TRUE),
    max_tbhiv_prct = max(e_tbhiv_prct, na.rm = TRUE)
  )

print(tb_hiv_summary)

# Correlation between TB incidence and mortality
correlation <- cor(TB$e_inc_100k, TB$e_mort_100k, use = "complete.obs")
print(paste("Correlation between TB incidence and mortality:", round(correlation, 3)))

# Scatter plot of TB incidence vs mortality
ggplot(TB, aes(x = e_inc_100k, y = e_mort_100k)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  theme_minimal() +
  labs(title = "TB Incidence vs Mortality",
       x = "Estimated Incidence per 100,000 population",
       y = "Estimated Mortality per 100,000 population")

# Regional analysis
regional_summary <- TB %>%
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
time_trend <- TB %>%
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
cfr_summary <- TB %>%
  summarise(
    mean_cfr = mean(cfr, na.rm = TRUE),
    median_cfr = median(cfr, na.rm = TRUE),
    min_cfr = min(cfr, na.rm = TRUE),
    max_cfr = max(cfr, na.rm = TRUE)
  )

print(cfr_summary)

# Histogram of Case Fatality Ratio
ggplot(TB, aes(x = cfr)) +
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
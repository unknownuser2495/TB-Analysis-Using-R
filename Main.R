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


# Linear regression model
model <- lm(best ~ year, data = TB_burden_age_sex)
summary(model)

# Box plot of measure by age group
ggplot(TB_burden_age_sex, aes(x = age_group, y = best)) +
  geom_boxplot() +
  labs(title = "Measure by Age Group", x = "Age Group", y = "Measure")


# Faceted bar plot of measure by sex and age group
ggplot(TB_burden_age_sex, aes(x = age_group, y = best, fill = sex)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ sex) +
  labs(title = "Measure by Sex and Age Group", x = "Age Group", y = "Measure")

# Summarize measure by country and year
measure_by_country_year <- TB_burden_age_sex %>%
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



            #For TB_expenditure_utilisation


# Check the imputed TB_expenditure_utilisation
head(TB_expenditure_utilisation_imputed_mean)


# Summarize total expenditure by country
exp_by_country <- TB_expenditure_utilisation %>%
  group_by(country) %>%
  summarize(total_exp = sum(exp_tot, na.rm = TRUE))

# Bar plot of total expenditure by country
ggplot(exp_by_country, aes(x = reorder(country, -total_exp), y = total_exp)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Total Expenditure by Country", x = "Country", y = "Total Expenditure")


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

# Linear regression model
model <- lm(exp_tot ~ rcvd_tot, TB_expenditure_utilisation = TB_expenditure_utilisation)
summary(model)


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

# Calculate the correlation matrix
cor_matrix <- cor(outcome_columns, use = "pairwise.complete.obs")

# Create a heatmap of the correlation matrix
corrplot(cor_matrix, method = "color", addCoef.col = "black", tl.col = "black", tl.srt = 45)

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

# Function to check and install packages if not already installed
install_if_missing <- function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# Load necessary libraries
install_if_missing("dplyr")
install_if_missing("ggplot2")
install_if_missing("readr")

# Load the dataset from the provided file path
data <- TB_outcomes_age_sex

# Check the structure of the dataset to identify the columns
str(data)

# Print column names to identify the correct ones
print(colnames(data))

# Select relevant columns for analysis (adjust based on actual column names in your data)
relevant_columns <- c("iso3", "age_group", "best", "risk_factor")

# Subset the dataset with relevant columns
tb_analysis <- data[, relevant_columns]

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
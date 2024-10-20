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
tb_data <- read.csv("C:\\Users\\Lenovo\\Desktop\\TB-Analysis-Using-R\\R files\\TB_burden_age_sex_2024-10-04.csv")

# Check the structure of the dataset to identify the columns
str(tb_data)

# Print column names to identify the correct ones
print(colnames(tb_data))

# Select relevant columns for analysis (adjust based on actual column names in your data)
relevant_columns <- c("iso3", "age_group", "best", "risk_factor")

# Subset the dataset with relevant columns
tb_analysis <- tb_data[, relevant_columns]

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

#Installing necessary libraries


#Loading necessary libraries


# Loading the data set
df = read.csv("TB.csv")


#Removing Null values by dropping them
data = na.omit(df)

#Viewing the size of the data after removing the null values
dim(data)

#Viewing overall summary of the data
summary(data)

# Define a function to remove outliers using Winsorization
remove_outliers <- function(df) {
  # Exclude columns that are not numerical or are not suitable for outlier handling
  exclude_cols <- c("country", "iso2", "iso3", "iso_numeric", "g_whoregion", "year", "e_pop_num", "cfr", "cfr_lo", "cfr_hi", "cfr_pct", "cfr_pct_lo", "cfr_pct_hi")
  
  # Apply Winsorization to each numeric column
  num_cols <- setdiff(names(df), exclude_cols)
  
  for (col in num_cols) {
    q1 <- quantile(df[[col]], 0.01)
    q99 <- quantile(df[[col]], 0.99)
    df[[col]][df[[col]] < q1] <- q1
    df[[col]][df[[col]] > q99] <- q99
  }
  
  return(df)
}

# Apply the function to each column of the data frame
data_winsorized <- data.frame(sapply(data, remove_outliers))

View(data_winsorized)

#Viewing overall summary of the data after handling outliers
summary(data)


# 2. Geographic Variations
region_incidence <- tapply(data$e_inc_100k, data$g_whoregion, mean, na.rm = TRUE)
barplot(region_incidence, main = "Incidence by Region", xlab = "Region", ylab = "Incidence Rate")


# Trends Over Time
trends <- aggregate(e_inc_100k ~ year, data = data, FUN = mean)
plot(trends$year, trends$incidence_rate, type = "l", xlab = "Year", ylab = "Incidence Rate")

# Geographic Variations   (Doesn't work rn)
geo_var <- aggregate(e_inc_100k ~ country, data = data, FUN = mean)
barplot(geo_var$incidence_rate, names.arg = geo_var$country, las = 2, xlab = "Country", ylab = "Incidence Rate")

# Risk Factors
#Not enough data
#model <- lm(e_inc_100k~ age + gender + hiv_status, data = data)
#summary(model)

# Treatment Effectiveness
#t.test(outcome ~ treatment, data = data)

# Impact of Public Health Interventions
#t.test(outcome ~ intervention, data = data)

# Age and Gender Disparities
#t.test(outcome ~ age_group, data = data)

# HIV Co-infection
#Not enough data
#model <- lm(incidence_rate ~ age + gender + hiv_status, data = data)
#summary(model)

# Socioeconomic Factors
#Not enough data
# model <- lm(e_inc_100k ~ income + education + urbanization, data = data)
# summary(model)

# Seasonal Variations
# Graph doesn't look right
# ts_data <- ts(data$e_inc_100k, frequency = 12)
# decomposition <- decompose(ts_data)
# plot(decomposition)

# Healthcare Access
#Insufficient data
# model <- lm(incidence_rate ~ healthcare_access + healthcare_infrastructure, data = data)
# summary(model)



# 1. Trends Over Time
trends <- aggregate(e_inc_100k ~ year, data = data, FUN = mean)
plot(trends$year, trends$e_inc_100k, type = "l", xlab = "Year", ylab = "Incidence Rate")

# 2. Geographic Variations
geo_var <- aggregate(e_inc_100k ~ g_whoregion, data = data, FUN = mean)
barplot(geo_var$e_inc_100k, names.arg = geo_var$g_whoregion, las = 2, xlab = "Region", ylab = "Incidence Rate")



# Load the data set
data <- read.csv("TB.csv")

# Removing Null values by dropping them
data <- na.omit(data)

# Viewing the size of the data after removing the null values
print(dim(data))

# Viewing overall summary of the data
print(summary(data))


# 3. Risk Factors
# Doesn't work rn
# model <- lm(e_inc_100k ~ e_prev_100k + e_mort_exc_tbhiv_100k, data = data)
# summary(model)

# 7. HIV Co-infection
model <- lm(e_inc_100k ~ e_tbhiv_prct, data = data)
summary(model)

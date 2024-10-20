# Load the dataset

TB_burden_age_sex = read.csv("Datasets/TB_burden_age_sex.csv")
TB_expenditure_utilisation = read.csv("Datasets/TB_expenditure_utilisation.csv")
TB_outcomes_age_sex = read.csv("Datasets/TB_outcomes_age_sex.csv")


# Convert data types if necessary

#For TB_burden_age_sex
TB_burden_age_sex$country <- as.factor(TB_burden_age_sex$country)
TB_burden_age_sex$year <- as.numeric(TB_burden_age_sex$year)
TB_burden_age_sex$measure <- as.factor(TB_burden_age_sex$measure)
TB_burden_age_sex$unit <- as.factor(TB_burden_age_sex$unit)
TB_burden_age_sex$age_group <- as.factor(TB_burden_age_sex$age_group)
TB_burden_age_sex$sex <- as.factor(TB_burden_age_sex$sex)
TB_burden_age_sex$risk_factor <- as.factor(TB_burden_age_sex$risk_factor)
TB_burden_age_sex$best <- as.numeric(TB_burden_age_sex$best)



TB_outcomes_age_sex <- TB_outcomes_age_sex %>%
  mutate(
    country = as.character(country),
    iso2 = as.character(iso2),
    iso3 = as.character(iso3),
    iso_numeric = as.numeric(iso_numeric),
    g_whoregion = as.character(g_whoregion),
    year = as.numeric(year),
    cohort_type = as.character(cohort_type),
    age_group = as.character(age_group),
    sex = as.character(sex),
    coh = as.numeric(coh),
    succ = as.numeric(succ),
    fail = as.numeric(fail),
    died = as.numeric(died),
    lost = as.numeric(lost),
    neval = as.numeric(neval),
    tsr = as.numeric(tsr)
  )




remove_outliers <- function(df) {
  # Identify numeric columns
  num_cols <- sapply(df, is.numeric)

  # Apply Winsorization to each numeric column
  for (col in names(df)[num_cols]) {
    q1 <- quantile(df[[col]], 0.01, na.rm = TRUE)
    q99 <- quantile(df[[col]], 0.99, na.rm = TRUE)
    df[[col]][df[[col]] < q1] <- q1
    df[[col]][df[[col]] > q99] <- q99
  }

  return(df)
}


# Apply the function to each column of the data frame
TB_burden_age_sex <- data.frame(sapply(TB_burden_age_sex, remove_outliers))
TB_expenditure_utilisation <- data.frame(sapply(TB_expenditure_utilisation, remove_outliers))
TB_outcomes_age_sex <- data.frame(sapply(TB_outcomes_age_sex, remove_outliers))


#Imputing the dataset

TB_burden_age_sex <- TB_burden_age_sex %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))

TB_expenditure_utilisation <- TB_expenditure_utilisation %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))

TB_outcomes_age_sex <- TB_outcomes_age_sex %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))
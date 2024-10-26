# Load the dataset

TB_burden_age_sex = read.csv("Datasets/TB_burden_age_sex.csv")
TB_expenditure_utilisation = read.csv("Datasets/TB_expenditure_utilisation.csv")
TB_outcomes_age_sex = read.csv("Datasets/TB_outcomes_age_sex.csv")
TB = read.csv("Datasets/TB.csv")

df <- TB_burden_age_sex
num_cols <- sapply(df, is.numeric)

# Apply Winsorization to each numeric column
for (col in names(df)[num_cols]) {
  q1 <- quantile(df[[col]], 0.01, na.rm = TRUE)
  q99 <- quantile(df[[col]], 0.99, na.rm = TRUE)
  df[[col]][df[[col]] < q1] <- q1
  df[[col]][df[[col]] > q99] <- q99
}

df <- TB_expenditure_utilisation
num_cols <- sapply(df, is.numeric)

# Apply Winsorization to each numeric column
for (col in names(df)[num_cols]) {
  q1 <- quantile(df[[col]], 0.01, na.rm = TRUE)
  q99 <- quantile(df[[col]], 0.99, na.rm = TRUE)
  df[[col]][df[[col]] < q1] <- q1
  df[[col]][df[[col]] > q99] <- q99
}

df <- TB_outcomes_age_sex 
num_cols <- sapply(df, is.numeric)

# Apply Winsorization to each numeric column
for (col in names(df)[num_cols]) {
  q1 <- quantile(df[[col]], 0.01, na.rm = TRUE)
  q99 <- quantile(df[[col]], 0.99, na.rm = TRUE)
  df[[col]][df[[col]] < q1] <- q1
  df[[col]][df[[col]] > q99] <- q99
}

df <- TB
num_cols <- sapply(df, is.numeric)

# Apply Winsorization to each numeric column
for (col in names(df)[num_cols]) {
  q1 <- quantile(df[[col]], 0.01, na.rm = TRUE)
  q99 <- quantile(df[[col]], 0.99, na.rm = TRUE)
  df[[col]][df[[col]] < q1] <- q1
  df[[col]][df[[col]] > q99] <- q99
}

#Imputing the dataset

TB_burden_age_sex <- TB_burden_age_sex %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))

TB_expenditure_utilisation <- TB_expenditure_utilisation %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))

TB_outcomes_age_sex <- TB_outcomes_age_sex %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))

TB <- TB %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))
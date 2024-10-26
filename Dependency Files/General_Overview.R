data_overview <- function(df) {
  # General overview
  print("General Overview:")
  print(ExpData(data = df, type = 1))

  # Check structure
  print("Structure:")
  print(str(df))

  # Summary statistics
  print("Summary Statistics:")
  print(summary(df))
}

data_overview(TB_burden_age_sex)
data_overview(TB_expenditure_utilisation)
data_overview(TB_outcomes_age_sex)
data_overview(TB)

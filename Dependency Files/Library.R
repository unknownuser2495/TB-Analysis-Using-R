# Function to check and install packages if not already installed
install_if_missing <- function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

list_of_libraries <- c("dplyr", "ggplot2", "readr", "SmartEDA", "ggcorrplot", 
                       "visdat", "tidyr", "corrplot", "scales", "tidyverse" )

# Load necessary libraries
sapply(list_of_libraries, install_if_missing)

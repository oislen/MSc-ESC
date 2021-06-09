# load relevant libraries
library('dplyr')
# load custom correlation test function
source("Scripts/utilities/correlation_tests.R")
# define function to apply chi-sq tests to given data and columns
data_reduction_corr <- function(dataset, col_names){
  # Subset the data to be tested
  sub_df <- dataset %>% subset(select = col_names)
  # Run the Chi-Squared Tests
  corr_tests <- correlation_tests(dataset = sub_df)
  # Filter out the significant p-values
  corr_tests_sign <- corr_tests[corr_tests$Correlation > 0.9,]
  # extract out the column names for significant columns
  corr_tests_remove_cols <- unique(corr_tests_sign[,'Y'])
  # return chi-sq columns to remove
  return(corr_tests_remove_cols)
}
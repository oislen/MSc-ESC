# load relevant libraries
library('dplyr')
# load custom chi-sq test function
source("Scripts/utilities/chisq_tests.R")
# define function to apply chi-sq tests to given data and columns
data_reduction_chisq <- function(dataset, col_names){
  # Subset the data to be tested
  sub_df <- dataset %>% subset(select = col_names)
  # Run the Chi-Squared Tests
  chisq_tests <- chisq_tests(dataset = sub_df)
  # Filter out the significant p-values
  chisq_tests_sign <- chisq_tests[chisq_tests$'P-Value' < 0.05,]
  # extract out the column names for significant columns
  chisq_test_remove_cols <- unique(chisq_tests_sign[,'Y'])
  # return chi-sq columns to remove
  return(chisq_test_remove_cols)
}
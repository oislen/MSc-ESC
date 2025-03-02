library('dplyr')
source("utilities/chisq_tests.R")

#' Data Reduction Chi-Square
#' 
#' @description Removes columns from a dataframe based on chi-squared tests of associations
#' 
#' @param dataset A dataframe with categorical columns
#' @param col_names The categorical columns to run chi-squared tests of associations
#' @param p_value The p value significance level to retain categorical columns with
#' 
#' @return Returns a dataframe with any columns having non-significant chi-square tests removed
#' 
data_reduction_chisq <- function(
    dataset,
    col_names,
    p_value=0.05
    ){
  # Subset the data to be tested
  sub_df <- dataset %>% subset(select = col_names)
  # Run the Chi-Squared Tests
  chisq_tests <- chisq_tests(dataset = sub_df)
  # Filter out the significant p-values
  chisq_tests_sign <- chisq_tests[chisq_tests$'P-Value' < p_value,]
  # extract out the column names for significant columns
  chisq_test_remove_cols <- unique(chisq_tests_sign[,'Y'])
  # return chi-sq columns to remove
  return(chisq_test_remove_cols)
}
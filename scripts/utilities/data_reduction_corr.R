library("dplyr")
source("utilities/correlation_tests.R")

#' Data Reduction Correlation
#' 
#' @description Removes columns from a dataframe based on correlation tests
#' 
#' @param dataset A dataframe with numeric columns
#' @param col_names The numeric columns to run correlation tests
#' @param corr_threshold The correlation threshold to retain numeric columns
#' 
#' @return Returns a dataframe with any columns having non-significant correlation tests removed
#' 
data_reduction_corr <- function(
    dataset,
    col_names,
    corr_threshold=0.9
    ){
  # Subset the data to be tested
  sub_df <- dataset %>% subset(select = col_names)
  # Run the Chi-Squared Tests
  corr_tests <- correlation_tests(dataset = sub_df)
  # Filter out the significant p-values
  corr_tests_sign <- corr_tests[corr_tests$Correlation > corr_threshold,]
  # extract out the column names for significant columns
  corr_tests_remove_cols <- unique(corr_tests_sign[,"Y"])
  # return chi-sq columns to remove
  return(corr_tests_remove_cols)
}
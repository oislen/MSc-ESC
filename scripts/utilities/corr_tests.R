#' Correlation Tests
#' 
#' @description Performs correlation tests for given columns in a dataframe against a specified target
#' 
#' @param data A dataframe with columns to perform correlation tests with
#' @param col_names A vector of column names to test for correlation
#' @param tar_col_name A string for the target column to use in the correlation tests, default is 'Points'
#' @param na.action A string indicating the action to take when dealing with na values, default is 'na.omit'
#' @param digit An integer indicating the number of digits to round the correlation test output to, default is 5
#' @param sign_level A float indicating the significance level to set in the correlation tests, default is 0.05
#' 
#' @return Returns a dataframe containing the correlation test results
#' 
corr_tests <- function(data, col_names, tar_col_name = "Points", na.action = "na.omit", digits = 5, sign_level = 0.05){
  # set the output column names
  output_cols <- c("X", "Y", "Correlation", "P-Value", "Significant")
  # Create a data frame to hold the correlation test data
  cor_test_df <- as.data.frame(matrix(nrow = length(col_names), ncol = length(output_cols), dimnames = list(col_names, output_cols)))
  # run a for loop to populate the data frame
  for (col in col_names) {
    # Perform Correlation Test
    c.t. <- cor.test(x = as.numeric(data[,col]), y = as.numeric(data$Points), na.action = na.action)
    # Fill in the X Variable Name
    cor_test_df[col, "X"] <- col
    # Fill in the Y Variable Name
    cor_test_df[col, "Y"] <- tar_col_name
    # Fill in the correlation
    cor_test_df[col, "Correlation"] <- round(c.t.$estimate, digits = digits)
    # Fill in the p-value
    cor_test_df[col, "P-Value"] <- round(c.t.$p.value, digits = digits)
    # Fill in the significance column
    cor_test_df[col, "Significant"] <- ifelse(test = round(c.t.$p.value, digits = digits) < sign_level, yes = TRUE, no = FALSE)
  }
  row.names(cor_test_df) <- NULL
  return(cor_test_df)
}
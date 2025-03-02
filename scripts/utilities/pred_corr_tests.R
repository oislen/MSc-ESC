#' Predictor Correlation Tests
#' 
#' @description Performs correlation tests for columns pairs in a dataframe
#' 
#' @param data A dataframe with columns to perform correlation tests with
#' @param na.action A string indicating the action to take when dealing with na values, default is 'na.omit'
#' @param digit An integer indicating the number of digits to round the correlation test output to, default is 5
#' @param sign_level A float indicating the significance level to set in the correlation tests, default is 0.05
#' 
#' @return Returns a dataframe containing the correlation test results
#' 
pred_corr_tests <- function (dataset, na.action = "na.omit", digits = 5, sign_level = 0.05) {
  # extract the unique column names
  n_cols = ncol(dataset)
  dataset_cols = unique(colnames(dataset))
  cor_test_df <- as.data.frame(matrix(ncol = 5))
  # Name the columns of the Correlation Test Data Frame
  colnames(cor_test_df) <- c("X", "Y", "Correlation", "P-Value", "Significant")
  # Create a row index to populate the data frame with
  r = 1
  for (i in 1:n_cols) {
    j = i + 1
    while (j <= n_cols) {
      # Perform Correlation Test
      c.t. <- cor.test(x = dataset[,dataset_cols[i]], y = dataset[,dataset_cols[j]], na.action = na.action)
      # Fill in the X Variable Name
      cor_test_df[r, "X"] <- dataset_cols[i]
      # Fill in the Y Variable Name
      cor_test_df[r, "Y"] <- dataset_cols[j]
      # Fill in the correlation
      cor_test_df[r, "Correlation"] <- round(c.t.$estimate, digits = digits)
      # Fill in the p-value
      cor_test_df[r, "P-Value"] <- round(c.t.$p.value, digits = digits)
      # Fill in significant result
      cor_test_df[r, "Significant"] <- ifelse(test = round(c.t.$p.value, digits = digits) < sign_level, yes = TRUE, no = FALSE)
      # Update the row index
      r = r + 1
      # Update the j index
      j = j + 1
    }
  }
  # return the cor_test_df
  row.names(cor_test_df) <- NULL
  return(cor_test_df)
}
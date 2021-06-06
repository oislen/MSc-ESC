# IMPORTANT: Correlation does not imply causality
# Define a function to perform Correlation Tests
pred_corr_tests <- function (dataset) {
  # extract the unique column names
  n_cols = ncol(dataset)
  dataset_cols = unique(colnames(dataset))
  cor_test_df <- as.data.frame(matrix(ncol = 4))
  # Name the columns of the Correlation Test Data Frame
  colnames(cor_test_df) <- c("X", "Y", "Correlation", "P-Value")
  # Create a row index to populate the data frame with
  r = 1
  for (i in 1:n_cols) {
    j = i + 1
    while (j <= n_cols) {
      # Perform Correlation Test
      c.t. <- cor.test(x = dataset[,dataset_cols[i]],
                       y = dataset[,dataset_cols[j]],
                       na.action = "na.omit")
      # Fill in the X Variable Name
      cor_test_df[r, 1] <- dataset_cols[i]
      # Fill in the Y Variable Name
      cor_test_df[r, 2] <- dataset_cols[j]
      # Fill in the correlation
      #cor_test_df[r, 3] <- round(c.t.$estimate, digits = 5)
      cor_test_df[r, 3] <- round(cor(x = dataset[,dataset_cols[i]],
                                     y = dataset[,dataset_cols[j]],
                                     use = "complete.obs"), 
                                 digits = 5)
      # Fill in the p-value
      cor_test_df[r, 4] <- round(c.t.$p.value, digits = 5)
      # Update the row index
      r = r + 1
      # Update the j index
      j = j + 1
    }
  }
  # return the cor_test_df
  return(cor_test_df)
}
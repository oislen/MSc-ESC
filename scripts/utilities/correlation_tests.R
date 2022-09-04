correlation_tests <- function (dataset) {
  # Create a data frame to hold the correlation test data
  cor_test_df <- as.data.frame(matrix(ncol = 4))
  # Name the columns of the Correlation Test Data Frame
  colnames(cor_test_df) <- c("X", "Y", "Correlation", "P-Value")
  # Create a row index to populate the data frame with
  r = 1
  for (i in 1:ncol(dataset)) {
    j = i + 1
    while (j  <= ncol(dataset)) {
      # Perform Correlation Test
      c.t. <- cor.test(x = dataset[,i],  y = dataset[,j], na.action = "na.omit")
      # Fill in the X Variable Name
      cor_test_df[r, 1] <- colnames(dataset)[i]
      # Fill in the Y Variable Name
      cor_test_df[r, 2] <- colnames(dataset)[j]
      # Fill in the correlation
      cor_test_df[r, 3] <- round(abs(c.t.$estimate), digits = 5)
      # Fill in the p-value
      cor_test_df[r, 4] <- round(c.t.$p.value, digits = 5)
      # Update the row index
      r = r + 1
      j = j + 1
    }
  }
  # return the cor_test_df
  return(cor_test_df)
}
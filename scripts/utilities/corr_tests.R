# Define a function to perform Correlation Tests
corr_tests <- function(data, col_names, na.action = "na.omit", digits = 5, sign_level = 0.05){
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
    cor_test_df[col, "Y"] <- "Points"
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
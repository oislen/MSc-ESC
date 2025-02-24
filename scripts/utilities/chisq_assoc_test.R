#' Chi-Square Tests of Association
#' 
#' @description Perform chi-sq tests of association with the target variable Points
#' 
#' @param data A dataframe with columns to perform chi-sq association tests with
#' @param col_names A vector of column names to test for chi-sq association tests
#' @param tar_col_name A string for the target column to use in the chi-sq association tests, default is 'Points'
#' @param digit An integer indicating the number of digits to round the chi-square association tests output to, default is 5
#' @param sign_level A float indicating the significance level to set in the ch-sq association tests, default is 0.05
#' 
#' @return Returns a dataframe containing the chi-sq association tests results
#' 
chisq_assoc_test <- function(data, col_names, tar_col_name = "Points", digits = 5, sign_level = 0.05) {

  # set the names of the chi-sq test results
  chisq_results <- c("X", "Y", "P-Value", "Significant")
  
  # create an empty output data frame to hold the chi-sq test results
  chisqtestdf <- as.data.frame(matrix(nrow = length(col_names), ncol = length(chisq_results), dimnames = list(col_names, chisq_results)))
  
  # Use a for loop to fill in the data frame
  for (col in col_names) {
  
    # perform chi-sq test of association with Points
    chisqtest_res <- stats::chisq.test(x = as.factor(data[,col]), y = as.factor(data$Points), simulate.p.value = TRUE)
    
    # Save the variables name being tested
    chisqtestdf[col, "X"] <- col
    
    # save the target variable to the data frame
    chisqtestdf[col, "Y"] <- tar_col_name
    
    # save the p-value rounded to given number of digits
    chisqtestdf[col, "P-Value"] <- round(x = chisqtest_res$p.value, digits = digits)
    
    # use if else logic to determine pass fail of chi-sq test
    chisqtestdf[col, "Significant"] <- ifelse(test = round(x = chisqtest_res$p.value, digits = digits) < sign_level, yes = TRUE, no = FALSE)
 
  }
  row.names(chisqtestdf) <- NULL
  return(chisqtestdf)
  
}
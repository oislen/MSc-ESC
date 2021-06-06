# define a function to perform chi-sq tests of association with the target variable Points
chisq_assoc_test <- function(data, col_names, digits = 5, sign_level = 0.05) {

  # set the names of the chi-sq test results
  chisq_results <- c("X", "Y", "P-Value", "Significant")
  
  # create an empty output data frame to hold the chi-sq test results
  chisqtestdf <- as.data.frame(matrix(nrow = length(col_names), ncol = length(chisq_results), dimnames = list(col_names, chisq_results)))
  
  # Use a for loop to fill in the data frame
  for (col in col_names) {
  
    # perform chi-sq test of association with Points
    chisqtest_res <- chisq.test(x = as.factor(data[,col]), y = as.factor(data$Points), simulate.p.value = TRUE)
    
    # Save the variables name being tested
    chisqtestdf[col, "X"] <- col
    
    # save the target variable to the data frame
    chisqtestdf[col, "Y"] <- "Points"
    
    # save the p-value rounded to given number of digits
    chisqtestdf[col, "P-Value"] <- round(x = chisqtest_res$p.value, digits = digits)
    
    # use if else logic to determine pass fail of chi-sq test
    chisqtestdf[col, "Significant"] <- ifelse(test = round(x = chisqtest_res$p.value, digits = digits) < sign_level, yes = "y", no = "n")
 
  }
  
  return(chisqtestdf)
  
}
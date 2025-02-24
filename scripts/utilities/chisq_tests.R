#' Chi-Square Tests of Association
#' 
#' @description Perform chi-sq tests of association against all column pair combinations in a given dataframe
#' 
#' @param data A dataframe with columns to perform chi-sq association tests with
#' @param digit An integer indicating the number of digits to round the chi-square association tests output to, default is 5
#' 
#' @return Returns a dataframe containing the chi-sq association tests results
#' 
chisq_tests <- function(dataset, digits = 5) {
  # create a data frame to store the relevant chi-squared test data
  data_frame_rows <- ncol(dataset)
  chisqtestdf <- as.data.frame(matrix(ncol = 5))
  # rename the columns of the data frame
  colnames(chisqtestdf) <- c("X", "Y", "X-Obs", "Y-Obs", "P-Value")
  # r represents the row index and will be used to input the relevant data
  r = 1
  for (i in 1:data_frame_rows) {
    j = i + 1
    while (j <= data_frame_rows) {
      # Save the variables name being tested
      chisqtestdf[r,1] <- colnames(dataset)[i]
      chisqtestdf[r,2] <- colnames(dataset)[j]
      # Input the number of observations
      chisqtestdf[r,3] <- apply(X = dataset, MARGIN = 2, FUN = sum)[i]
      chisqtestdf[r,4] <- apply(X = dataset,  MARGIN = 2, FUN = sum)[j]
      # Conduct the chi-squared test and save the p-value
      chisq_test_res <- chisq.test(x = as.factor(dataset[,i]), y = as.factor(dataset[,j]), simulate.p.value = TRUE)
      chisqtestdf[r,5] <- round(x = chisq_test_res$p.value, digits = digits)
      r = r + 1
      j = j + 1
    }
  }
  # Set the output of the function to be the chi-squared test data frame
  return(chisqtestdf)
}

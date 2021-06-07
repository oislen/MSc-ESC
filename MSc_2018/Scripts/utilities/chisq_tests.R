# Define a Function to preform chi-squared tests and store the results as a data frame
chisq_tests <- function(dataset) {
  # FUNCTION OVERVIEW
  # this function takes in a data set of purely categorical variables
  # and applies a chi-squared test of association between each one
  # There are two parts to it
  # (1) Create the data frame to store the chi-squared tests
  # (2) Fill the data frame with the relevant information
  # The underlying concept is that is valade to remove a variable if
  # (1) It is highly associated with another variable
  # (2) It is sparse
  #-- PART 1
  # First create a data frame to store the relevant chi-squared test data
  data_frame_rows <- ncol(dataset)
  chisqtestdf <- as.data.frame(matrix(ncol = 5))
  # rename the columns of the data frame
  colnames(chisqtestdf) <- c("X", "Y", "X-Obs", "Y-Obs", "P-Value")
  #-- PART 2
  # r represents the row index and will be used to input the relevant data
  r = 1
  for (i in 1:data_frame_rows) {
    j = i + 1
    while (j <= data_frame_rows) {
      # Save the variables name being tested
      chisqtestdf[r,1] <- colnames(dataset)[i]
      chisqtestdf[r,2] <- colnames(dataset)[j]
      # Input the number of observations
      chisqtestdf[r,3] <- apply(X = dataset, 
                                MARGIN = 2,
                                FUN = sum)[i]
      chisqtestdf[r,4] <- apply(X = dataset, 
                                MARGIN = 2,
                                FUN = sum)[j]
      # Conduct the chi-squared test and savethe p-value
      chisqtestdf[r,5] <- round(x = chisq.test(x = as.factor(dataset[,i]),
                                               y = as.factor(dataset[,j]))$p.value,
                                digits = 5)
      r = r + 1
      j = j + 1
    }
  }
  # Set the output of the function to be the chi-squared test dataframe
  return(chisqtestdf)
}

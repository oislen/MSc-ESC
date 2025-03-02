source("utilities/range_normalisation.R")

#' Range Standardise Data
#' 
#' @description Range standardise all data in a dataframe to have desired range
#' 
#' @param dataset A dataframe with numeric columns to range standardise
#' @param lb The lower bound of the normalisation range, default is 0
#' @param ub The upper bound of the normalisation range, default is 1
#' 
#' @return Returns the range standardised numeric dataframe
#' 
range_standardise_data <- function(
    dataset,
    lower_bound=0,
    upper_bound=1
    ) {
  standardised_data <- dataset
  # data set: the specified data set to range standardized
  # the inner function defines the range standardization transform, as R does not have one
  # the outer function applies range standardization to the numeric variables of the specified data set
  for (i in 1:ncol(dataset)) {
    if (is.numeric(dataset[,i])){
      standardised_data[,i] <- range_normalisation(dataset = dataset[,i], lb = lower_bound, ub = upper_bound)
    }
  }
  return(standardised_data)
}
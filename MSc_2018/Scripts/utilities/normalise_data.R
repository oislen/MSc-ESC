# (2) DATA NORMALISATION TO MEAN 0 AND STANDARD DEVIATION 1
normalise_data <- function(dataset) {
  standardised_data <- dataset
  # data set: the specified data set to range standardized
  # the inner function defines the range standardization transform, as R does not have one
  # the outer function applies range standardization to the numeric variables of the specified data set
  data_normalisation <- function(dataset) {
    # range standardization [lower bound, upper_bound], e.g. [0, 1]
    # Convert data to have minimum 0 and maximum 1
    mean <- mean(dataset)
    standard.deviation <- sd(dataset)
    ((dataset - mean) / standard.deviation)
  }
  for (i in 1:ncol(dataset)) {
    if (is.numeric(dataset[,i])){
      standardised_data[,i] <- data_normalisation(dataset = dataset[,i])
    }
  }
  return(standardised_data)
}
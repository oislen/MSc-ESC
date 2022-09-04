# (1) RANGE STANDARDISATION TO [0,1]
range_standardise_data <- function(dataset, lower_bound = 0, upper_bound = 1) {
  standardised_data <- dataset
  # data set: the specified data set to range standardized
  # the inner function defines the range standardization transform, as R does not have one
  # the outer function applies range standardization to the numeric variables of the specified data set
  range_normalisation <- function(dataset, lb, ub) {
    # range standardization [lower bound, upper_bound], e.g. [0, 1]
    # Convert data to have minimum 0 and maximum 1
    mx <- max(dataset)
    mn <- min(dataset)
    ((((dataset - mn) / (mx - mn)) * (ub - lb)) + lb)
  }
  for (i in 1:ncol(dataset)) {
    if (is.numeric(dataset[,i])){
      standardised_data[,i] <- range_normalisation(dataset = dataset[,i], lb = lower_bound, ub = upper_bound)
    }
  }
  return(standardised_data)
}
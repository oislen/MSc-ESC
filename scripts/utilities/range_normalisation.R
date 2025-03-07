#' Range Normalisation
#' 
#' @description Normalises all data in a dataframe to be within the specified range
#' 
#' @param dataset A dataframe with numeric columns to range normalise
#' @param lb The lower bound of the normalisation range, default is 0
#' @param ub The upper bound of the normalisation range, default is 1
#' 
#' @return Returns the normalised numeric dataframe
#' 
range_normalisation <- function(
    dataset,
    lb=0,
    ub=1
    ) {
  # range standardisation [lower bound, upper_bound], e.g. [0, 1]
  # Convert data to have minimum 0 and maximum 1
  mx <- max(dataset)
  mn <- min(dataset)
  normalised_dataset <- ((((dataset - mn) / (mx - mn)) * (ub - lb)) + lb)
  return(normalised_dataset)
}
# (1) Define the range standardization
range_normalisation <- function(dataset, lb, ub) {
  # range standardisation [lower bound, upper_bound], e.g. [0, 1]
  # Convert data to have minimum 0 and maximum 1
  mx <- max(dataset)
  mn <- min(dataset)
  ((((dataset - mn) / (mx - mn)) * (ub - lb)) + lb)
}
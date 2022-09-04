# (2) Define the data normalization
data_normalisation <- function(dataset) {
  # range standardization [lower bound, upper_bound], e.g. [0, 1]
  # Convert data to have minimum 0 and maximum 1
  mean <- mean(dataset)
  standard.deviation <- sd(dataset)
  ((dataset - mean) / standard.deviation)
}
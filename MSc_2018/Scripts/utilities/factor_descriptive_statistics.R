# load required libraries
library(moments)

# define a function to calculate descriptive statistics for categorical variables
factor_descriptive_statistics <- function(dataset, digits = 2) {
  # function that automatically prints relevant descriptive statistics for attributes in a given data set
  # create the data frame to hold the categorical descriptive statistics
  # set the name of the statistics being generated
  stats_names <- c("nlevels", "1st mode", "1st mode %", "2nd mode", "2nd mode %", "NA %")
  # extract out the character columns
  factor_cols <- names(which(sapply(X = dataset, FUN = function(x) is.factor(x))))
  # create a data frame of empty
  factor_desc_stats <- as.data.frame(matrix(nrow = length(factor_cols), ncol = length(stats_names), dimnames = list(factor_cols, stats_names)))
  # iterate through each factor column
  for (col in factor_cols) {
    # next compute the descriptive statistics for the factor column
    factor_desc_stats[col, "nlevels"] <- nlevels(dataset[, col])
    factor_desc_stats[col, "1st mode"] <- names(which.max(summary(dataset[, col])))
    factor_desc_stats[col, "1st mode %"] <- round(summary(dataset[, col])[which.max(summary(dataset[, col]))] * 100 / sum(summary(dataset[, col])), digits = digits)
    factor_desc_stats[col, "2nd mode"] <- names(sort(summary(dataset[, col]), decreasing = TRUE)[2])
    factor_desc_stats[col, "2nd mode %"] <- round(sort(summary(dataset[, col]), decreasing = TRUE)[2] * 100 / sum(summary(dataset[, col])), digits = digits)
    factor_desc_stats[col, "NA %"] <- length(which(is.na(dataset[, col]))) * 100 / sum(summary(dataset[, col]))
    
  }
  return(factor_desc_stats)
}

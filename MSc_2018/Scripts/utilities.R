# load required libraries
library(moments)

# define a function to convert of the columns of a data frame to factor variables
column_to_factor <- function(dataset, col_names){
  # loop through the columns to be converted
  for (col in col_names){
    # convert the column to factor
    dataset[, col] <- as.factor(dataset[, col])
  }
  return(dataset)
}

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

# define a function to calculate descriptive statistics for continuous variables
numeric_descriptive_statistics <- function(dataset, digits = 3, na.rm = TRUE) {
  # IMPORTANT NOTE: all the descriptive statistics are by default calculated with the NA values removed
  # function that automatically prints relevant descriptive statistics for attributes in a given data set
  # create the data frame to hold the numeric descriptive statistics
  # set the name of the statistics being generated
  stats_names <- c("mean", "variance", "min", "max", "range", "NA %")
  # extract out the character columns
  numeric_cols <- names(which(sapply(X = dataset, FUN = function(x) is.numeric(x))))
  # create a data frame of empty
  numeric_desc_stats <- as.data.frame(matrix(nrow = length(numeric_cols), ncol = length(stats_names), dimnames = list(numeric_cols, stats_names)))
  # iterate through each numeric column
  for (col in numeric_cols) {
    # next compute the descriptive statistics for the numeric column
    numeric_desc_stats[col, "mean"] <- round(mean(dataset[, col], na.rm = na.rm), digits = digits)
    numeric_desc_stats[col, "variance"] <- round(var(dataset[, col], na.rm = na.rm), digits = digits)
    numeric_desc_stats[col, "min"] <- round(min(dataset[, col], na.rm = na.rm), digits = digits)
    numeric_desc_stats[col, "max"] <- round(max(dataset[, col], na.rm = na.rm), digits = digits)
    numeric_desc_stats[col, "range"] <- round(max(dataset[, col], na.rm = na.rm) - min(dataset[, col], na.rm = na.rm), digits = digits)
    numeric_desc_stats[col, "NA %"] <- round(length(which(is.na(dataset[, col]))) * 100 / nrow(dataset), digits = digits)
  }
  return(numeric_desc_stats)
}


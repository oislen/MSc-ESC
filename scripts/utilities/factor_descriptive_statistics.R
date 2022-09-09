# load required libraries
library(moments)

# define a function to calculate descriptive statistics for categorical variables
factor_descriptive_statistics <- function(dataset, col_names, digits = 2) {
  # function that automatically prints relevant descriptive statistics for attributes in a given data set
  # create the data frame to hold the categorical descriptive statistics
  # set the name of the statistics being generated
  stats_names <- c("nlevels", "1st mode", "1st mode %", "2nd mode", "2nd mode %", "NA %")
  # create a data frame of empty
  factor_desc_stats <- as.data.frame(matrix(nrow = length(col_names), ncol = length(stats_names), dimnames = list(col_names, stats_names)))
  # iterate through each factor column
  for (col in col_names) {
    # next compute the descriptive statistics for the factor column
    factor_desc_stats[col, "nlevels"] <- nlevels(as.factor(dataset[, col]))
    factor_desc_stats[col, "1st mode"] <- names(which.max(summary(as.factor(dataset[, col]))))
    factor_desc_stats[col, "1st mode %"] <- round(summary(as.factor(dataset[, col]))[which.max(summary(as.factor(dataset[, col])))] * 100 / sum(summary(as.factor(dataset[, col]))), digits = digits)
    factor_desc_stats[col, "2nd mode"] <- names(sort(summary(as.factor(dataset[, col])), decreasing = TRUE)[2])
    factor_desc_stats[col, "2nd mode %"] <- round(sort(summary(as.factor(dataset[, col])), decreasing = TRUE)[2] * 100 / sum(summary(as.factor(dataset[, col]))), digits = digits)
    factor_desc_stats[col, "NA %"] <- round(length(which(is.na(as.factor(dataset[, col])))) * 100 / sum(summary(as.factor(dataset[, col]))), digits)
    
  }
  return(factor_desc_stats)
}

library(moments)

#' Numeric Descriptive Statistics
#' 
#' @description Generates descriptive statistics for continuous variables within a dataframe
#' 
#' @param dataset The dataframe with continuous variables
#' @param col_names The continuous variables to generate numeric descriptive statistics for
#' @param digits The number of digits to round the descriptive statistics to, default is 3
#' @param na.rm Whether to remove NA values from the dataset when generating the numeric descriptive statistics, default is True
#' 
#' @return Returns the numeric descriptive statistics as a dataframe
#' 
numeric_descriptive_statistics <- function(dataset, col_names, digits = 3, na.rm = TRUE) {
  # create the data frame to hold the numeric descriptive statistics
  stats_names <- c("mean", "stdev", "min", "max", "range", "NA %")
  # create a data frame of empty
  numeric_desc_stats <- as.data.frame(matrix(nrow = length(col_names), ncol = length(stats_names), dimnames = list(col_names, stats_names)))
  # iterate through each numeric column
  for (col in col_names) {
    # next compute the descriptive statistics for the numeric column
    numeric_desc_stats[col, "mean"] <- round(mean(dataset[, col], na.rm = na.rm), digits = digits)
    numeric_desc_stats[col, "stdev"] <- round(sd(dataset[, col], na.rm = na.rm), digits = digits)
    numeric_desc_stats[col, "min"] <- round(min(dataset[, col], na.rm = na.rm), digits = digits)
    numeric_desc_stats[col, "max"] <- round(max(dataset[, col], na.rm = na.rm), digits = digits)
    numeric_desc_stats[col, "range"] <- round(max(dataset[, col], na.rm = na.rm) - min(dataset[, col], na.rm = na.rm), digits = digits)
    numeric_desc_stats[col, "NA %"] <- round(length(which(is.na(dataset[, col]))) * 100 / nrow(dataset), digits = digits)
  }
  return(numeric_desc_stats)
}


library(testthat)
setwd("../..")
source("./utilities/column_to_factor.R")

# create tests data
test_data <- read.csv(file = "../data/ESC_2016_voting_data.csv", header = TRUE, nrows=5)

# generate observed test data
to_factor_cols <- c("FC_LANGFAM","TC_LANGFAM")
obs_results <- column_to_factor(dataset=test_data, col_names=to_factor_cols)

testthat::test_that(
  "Column to Factor Function Tests",
  {
    expect_equal(typeof(obs_results), typeof(test_data))
    expect_equal(dim(obs_results), dim(test_data))
    expect_equal(colnames(obs_results), colnames(test_data))
    expect_true(all(sapply(obs_results[to_factor_cols], class) == "factor"))
    expect_equal(sapply(test_data[, -which(names(test_data) %in% to_factor_cols)], class), sapply(obs_results[, -which(names(obs_results) %in% to_factor_cols)], class))
  }
)

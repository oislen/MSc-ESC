library(testthat)
setwd("../..")
source("./utilities/range_standardise_data.R")

# create tests data
test_data <- read.csv(file = "../data/ESC_2016_voting_data.csv", header = TRUE, nrows=5)

# generate observed test data
obs_results <- range_standardise_data(dataset=test_data)

testthat::test_that(
  "Range Standardise Data Function Tests",
  {
    expect_equal(typeof(obs_results), typeof(test_data))
    expect_equal(dim(obs_results), dim(test_data))
    expect_equal(colnames(obs_results), colnames(test_data))
  }
)

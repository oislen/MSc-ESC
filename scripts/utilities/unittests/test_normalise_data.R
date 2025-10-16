library(testthat)
setwd("../..")
source("./utilities/normalise_data.R")

# create tests data
test_data <- read.csv(file = "../data/ESC_2016_voting_data.csv", header = TRUE, nrows=5)

# generate observed test data
obs_results <- normalise_data(dataset=test_data)

testthat::test_that(
  "Normalise Data Function Tests",
  {
    expect_equal(typeof(obs_results), typeof(test_data))
    expect_equal(dim(obs_results), dim(test_data))
    expect_equal(colnames(obs_results), colnames(test_data))
  }
)

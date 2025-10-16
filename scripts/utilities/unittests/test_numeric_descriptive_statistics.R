library(testthat)
setwd("../..")
source("./utilities/numeric_descriptive_statistics.R")

# create tests data
test_data <- read.csv(file = "../data/ESC_2016_voting_data.csv", header = TRUE, nrows=5)

# generate observed test data
obs_results <- numeric_descriptive_statistics(dataset=test_data, col_names=c("Points", "OOA"))

# set expected data
exp_colnames <- c("mean", "stdev", "min", "max", "range", "NA %")

testthat::test_that(
  "Numeric Descriptive Statistics Function Tests",
  {
    expect_equal(typeof(obs_results), typeof(test_data))
    expect_equal(nrow(obs_results), 2)
    expect_equal(ncol(obs_results), 6)
    expect_equal(colnames(obs_results), exp_colnames)
  }
)

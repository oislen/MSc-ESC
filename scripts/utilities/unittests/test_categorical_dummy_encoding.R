library(testthat)
setwd("../..")
source("./utilities/categorical_dummy_encoding.R")

# create tests data
test_data <- read.csv(file="../data/ESC_2016_voting_data.csv", header=TRUE, nrows=5)
test_data <- test_data[,c("id", "From_country","To_country")]

# generate observed test data
obs_results <- categorical_dummy_encoding(dataset=test_data)

testthat::test_that(
  "Categorical Dummy Encoding Function Tests",
  {
    expect_equal(typeof(obs_results), typeof(test_data))
    expect_equal(nrow(obs_results), nrow(test_data))
    expect_equal(ncol(obs_results), 7)
  }
)

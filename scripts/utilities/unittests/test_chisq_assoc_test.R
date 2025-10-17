library(testthat)
setwd("../..")
source("./utilities/chisq_assoc_test.R")

# create tests data
test_data <- read.csv(file="../data/ESC_2016_voting_data.csv", header=TRUE, nrows=5)

# generate observed test data
obs_results <- chisq_assoc_test(data=test_data, col_names=c("From_country","FC_LANGFAM"))

testthat::test_that(
  "Chi-Square Association Test Function Tests",
  {
    expect_equal(typeof(obs_results), "list")
    expect_equal(class(obs_results), "data.frame")
    expect_equal(nrow(obs_results), 2)
    expect_equal(ncol(obs_results), 4)
    expect_equal(colnames(obs_results), c("X", "Y", "P-Value", "Significant"))
  }
)

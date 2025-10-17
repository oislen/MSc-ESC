library(testthat)
setwd("../..")
source("./utilities/extract_preds_by_cats.R")

# generate observed test data
obs_results <- extract_preds_by_cats(cat="competition")

testthat::test_that(
  "Extract Predictions by Category Function Tests",
  {
    expect_equal(class(obs_results), "character")
    expect_equal(typeof(obs_results), "character")
    expect_equal(length(obs_results), 6)
  }
)

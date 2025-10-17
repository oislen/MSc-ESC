library(testthat)
setwd("../..")
source("./utilities/bonferroni_outlier_test.R")

# create test model
test_model <- readRDS("../models/overall_final_model.RDS")

# generate observed test data
obs_results <- bonferroni_outlier_test(model=test_model)

testthat::test_that(
  "Bonferroni Outlier Function Tests",
  {
    expect_equal(class(obs_results), "data.frame")
    expect_equal(nrow(obs_results), 1)
    expect_equal(ncol(obs_results), 5)
    expect_equal(colnames(obs_results), c("rstudent", "p", "bonf.p", "signif", "cutoff"))
  }
)

library(testthat)
setwd("../..")
source("./utilities/extract_factor_data.R")

# create tests data
test_data <- read.csv(file = "../data/ESC_2016_voting_data.csv", header = TRUE, nrows=5)

# generate observed test data
obs_results <- extract_factor_data(dataset=test_data)

# set expected data
exp_colnames <- c("From_country", "To_country", "Round", "Voting_Method", "Host_Nation", "ComVBlocs1", "ComVBlocs2", "FC_LANGFAM", "TC_LANGFAM", "ComLANGFAM", "Neighbours", "TC_PerfType", "TC_SingerGender", "FC_SONGLANG", "TC_SONGLANG")

testthat::test_that(
  "Extract Factor Data Function Tests",
  {
    expect_equal(typeof(obs_results), typeof(test_data))
    expect_equal(nrow(obs_results), nrow(test_data))
    expect_equal(ncol(obs_results), 15)
    expect_equal(colnames(obs_results), exp_colnames)
  }
)

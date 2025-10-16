library(testthat)
setwd("../..")
source("./utilities/extract_numeric_data.R")

# create tests data
test_data <- read.csv(file = "../data/ESC_2016_voting_data.csv", header = TRUE, nrows=5)

# generate observed test data
obs_results <- extract_numeric_data(dataset=test_data)

# set expected data
exp_colnames <- c("id", "Points", "OOA", "Average_Points", "VBlocs1_FC", "VBlocs2_FC", "VBlocs1_TC", "VBlocs2_TC", "TC_NumNeigh", "FC_NonCOB", "FC_NonCitzens", "FC_COB", "FC_Citizens", "FC_Population", "METRIC_COB", "METRIC_Citizens", "METRIC_COBCit", "FC_GDP_mil", "TC_GDP_mil", "GDP_PROP", "FC_CAP_LAT", "FC_CAP_LON", "TC_CAP_LAT", "TC_CAP_LON", "CAP_DIST_km", "ComSONGLAN")

testthat::test_that(
  "Extract Numeric Data Function Tests",
  {
    expect_equal(typeof(obs_results), typeof(test_data))
    expect_equal(nrow(obs_results), nrow(test_data))
    expect_equal(ncol(obs_results), 26)
    expect_equal(colnames(obs_results), exp_colnames)
  }
)

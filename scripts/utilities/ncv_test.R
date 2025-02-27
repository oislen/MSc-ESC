#' Non-Constant Variance Test
#' 
#' @description Performs a non-constant error variance test for a linear model
#' 
#' @param model The linear model to perform a non-constant error variance test
#' @param digits The number of digits to round the non-constant error variance test results to, default is 5
#' 
#' @return Returns the non-constant error variance tests results as a dataframe
#' 
ncv_test <- function(model, digits = 5) {
  ncv_test <- car::ncvTest(model)
  ncv_test_stat <- round(ncv_test$ChiSquare, digits)
  ncv_test_pval <- round(ncv_test$p, digits)
  ncv_test_name <- ncv_test$test
  ncv_test_vec <- c(ncv_test_stat, ncv_test_pval)
  names(ncv_test_vec) <- c("Chi-Sq Statistic", "P-Value")
  ncv_test_df <- as.data.frame(ncv_test_vec)
  colnames(ncv_test_df) <- c(ncv_test_name)
  return(ncv_test_df)
}
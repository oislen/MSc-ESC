#' Normality Test
#' 
#' @description Performs a selection of normality tests on studentized model residuals
#' 
#' @param sresid The studentized residuals to perform normality tests on
#' @param digits The number of digits to round the normality test results to, default is 5
#' 
#' @return Returns the normality tests results as a dataframe
#' 
normality_tests <- function(sresid, digits = 5) {
  # Shapiro normality test
  shapiro_test <- shapiro.test(sresid)
  shapiro_test_stat <- shapiro_test$statistic
  shapiro_test_pval <- shapiro_test$p.value
  # Anderson-Darling test
  ad_test <- ad.test(sresid)
  ad_test_stat <- ad_test$statistic
  ad_test_pval <- ad_test$p.value
  # Cramer-Von Mises test
  cvm_test <- cvm.test(sresid)
  cvm_test_stat <- cvm_test$statistic
  cvm_test_pval <- cvm_test$p.value
  # Lille test 
  lille_test <- lillie.test(sresid)
  lille_test_stat <- lille_test$statistic
  lille_test_pval <- lille_test$p.value
  # Person test
  person_test <- pearson.test(sresid)
  person_test_stat <- person_test$statistic
  person_test_pval <- person_test$p.value
  # Shapiro-Francia test
  sf_test <- sf.test(sresid)
  sf_test_stat <- sf_test$statistic
  sf_test_pval <- sf_test$p.value
  # create column vectors
  norm_tests_names <- c("shapiro.test", "ad.test", "cvm.test", "lille.test", "person.test", "sf.test")
  norm_tests_stats <- round(as.double(c(shapiro_test_stat, ad_test_stat, cvm_test_stat, lille_test_stat, person_test_stat, sf_test_stat)), digits)
  norm_tests_pvals <- round(c(shapiro_test_pval, ad_test_pval, cvm_test_pval, lille_test_pval, person_test_pval, sf_test_pval), digits)
  # create dataframe of normality statistics
  norm_tests_df <- as.data.frame(cbind(norm_tests_names, norm_tests_stats, norm_tests_pvals))
  colnames(norm_tests_df) <- c("NormTest", "Stat", "Pval")
  return(norm_tests_df)
}
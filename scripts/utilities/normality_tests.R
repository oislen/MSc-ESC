normality_tests <- function(sresid, digits = 5) {
  # shapiro normality test
  shapiro_test <- shapiro.test(sresid)
  shapiro_test_stat <- shapiro_test$statistic
  shapiro_test_pval <- shapiro_test$p.value
  # anderson darling test
  ad_test <- ad.test(sresid)
  ad_test_stat <- ad_test$statistic
  ad_test_pval <- ad_test$p.value
  # cramer-von mises test
  cvm_test <- cvm.test(sresid)
  cvm_test_stat <- cvm_test$statistic
  cvm_test_pval <- cvm_test$p.value
  # lille test 
  lille_test <- lillie.test(sresid)
  lille_test_stat <- lille_test$statistic
  lille_test_pval <- lille_test$p.value
  # person test
  person_test <- pearson.test(sresid)
  person_test_stat <- person_test$statistic
  person_test_pval <- person_test$p.value
  # shapiro-francia test
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

bonferroni_outlier_test <- function(model) {
  outlier_test <- outlierTest(model)
  outlier_test_vec <- c(as.double(outlier_test$rstudent), as.double(outlier_test$p), as.double(outlier_test$bonf.p), outlier_test$signif, outlier_test$cutoff)
  outlier_test_cols <- c("rstudent", "p", "bonf.p", "signif", "cutoff")
  names(outlier_test_vec) <- outlier_test_cols
  outlier_test_df <- data.frame(t(outlier_test_vec), row.names = names(outlier_test$rstudent))
  return(outlier_test_df)
}
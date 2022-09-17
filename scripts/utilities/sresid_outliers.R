sresid_outliers <- function(sresid, cutoff = -2) {
    sresid_df <- as.data.frame(names(which(sresid < cutoff)))
    colnames(sresid_df) = c("outlier_residuals")
    return(sresid_df)
}
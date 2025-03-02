#' Studentised Residual Outliers
#' 
#' @description Extracts outliers from studentised residuals
#' 
#' @param sresid The studentised residuals to extract outliers form
#' @param cutoff The cut off threshold to extract outliers from, default is -2
#' 
#' @return Returns the outlier studentised residuals
#' 
sresid_outliers <- function(
    sresid,
    cutoff=-2
    ) {
    sresid_df <- as.data.frame(names(which(sresid < cutoff)))
    colnames(sresid_df) = c("outlier_residuals")
    return(sresid_df)
}
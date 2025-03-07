library(dummy)

#' Categorical Dummy encoding
#' 
#' @description Generates dummy encoding for categorical variables within a dataframe
#' 
#' @param dataset A dataframe with categorical variables to dummy encode
#' 
#' @return Returns the dummy encode categorical variables as a dataframe
#' 
categorical_dummy_encoding <- function(dataset){
  # first subset categorical variables
  cvariables <- categories(x = dataset)
  # dummy encode the categorical variables
  dcvariables <- dummy(x = dataset, object = cvariables)
  # we are turning all categorical variables into numeric binary dummy variables
  # as such these numeric binary dummy variables cannot be defined as categorical variables
  # as regression functions cannot handle categorical variables
  # turn all variables into double precision numeric variables
  for (i in 1:length(dcvariables)) {
    dcvariables[,i] <- as.integer(dcvariables[,i])
  }
  # convert dummy encoded variables to a data frame
  dcvariables <- as.data.frame(x = dcvariables)
  # subset continuous variables
  # use set operations
  nvariables <- dataset[,setdiff(x = attributes(dataset)$names, y = attributes(cvariables)$names)]
  dummy_encoded_data <- as.data.frame(cbind(nvariables, dcvariables))
  return(dummy_encoded_data)
}
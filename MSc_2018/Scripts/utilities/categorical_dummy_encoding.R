# we need to dummy encode the categorical variables
library(dummy)
# define function to perform dummy encoding
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
    dcvariables[,i] <- as.double(dcvariables[,i])
  }
  dcvariables[dcvariables == 1] <- 0
  dcvariables[dcvariables == 2] <- 1
  dcvariables <- as.data.frame(x = dcvariables)
  # subset continuous variables
  # use set operations
  nvariables <- dataset[,setdiff(x = attributes(dataset)$names, y = attributes(cvariables)$names)]
  dummy_encoded_data <- as.data.frame(cbind(nvariables, dcvariables))
  return(dummy_encoded_data)
}
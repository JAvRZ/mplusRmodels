library(mice)

impute.data <- function(file = "exampledata.dat") {
  mydata <- read.table(file, na.strings = ".")
  # quickpred finds variables that are correlated (default is > 0.1) so can be used in the imputations
  mypred <- quickpred(mydata)
  
  # the quickpred functions may take a while, so it is worthwhile to work with other objects, keeping the quickpred object unchanged
  mypred2l <- mypred
  # In the example dataset, the 4th, 5th and 6th variables are sex, age and level of education,
  #   which in neuropsychology are generally informative, so these are explicitly added to the imputation models
  mypred2l[, c(4, 5, 6)] <- 1
  # study number denotes the second level, so this is made -2 for all 
  mypred2l[, 2] <- -2
  # Make explicit that variables cannot be used to impute themselves
  diag(mypred2l) <- 0
  
  # ID number (first column) could theoretically be correlated with outcomes, but should not be used in imputation models
  mypred2l[, 1] <- 0
  # ID number and study number do not need to be imputed if missing
  mypred2l[c(1, 2), ] <- 0
  
  # Multiple imputations are run, with more iterations and more datasets than is default
  # The method for imputations is 2-level for all tests
  mytempmi <- mice( m = 50, maxit = 10, mydata, predictorMatrix = mypred2l, method = "2l.pan")
  # The complete datasets are saved to the harddrive (with a similar name as the original file)
  mids2mplus(mytempmi, file.prefix = gsub(".dat", "_imp", file, fixed = TRUE))
}

# Function call
setwd("/example/")
impute.data("exampledata.dat")

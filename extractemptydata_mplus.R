library(MplusAutomation)

extractemptydata_mplus <- function(mplusoutputfile, covariates = c("SEX", "AGE", "EDU"), no.patients = 1){
  #' Construct a vector to fill in for a patient from Mplus output
  #'
  #' @param an output file generated with a two-level model in Mplus,
  #' with intercepts and variables regressing on covariates.
  #'
  #' @return A empty vector in which data can be entered
  #' @export
  #'
  #' @examples
  #' extractemptyvector_mplus( mplusoutputfile = "twolevelunstructmod.out", covariates = c("SEX", "AGE", "EDU"), no.patients = 1)

  modelparams <- extractModelParameters(target = mplusoutputfile)$unstandardized
  varnames <- modelparams$param[ modelparams$paramHeader == "Residual.Variances" & modelparams$BetweenWithin == "Within"]
  emptydata <- matrix( NA, nrow = no.patients, ncol = length(covariates) + length(varnames))
  colnames( emptydata) <- c( covariates, varnames )
  return(emptydata)
}




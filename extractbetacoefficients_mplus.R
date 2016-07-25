library(MplusAutomation)

extractbetacoefficients_mplus <- function(mplusoutputfile, covariates = c("SEX", "AGE", "EDU")){
  #' Construct a vector of beta coefficients from Mplus output
  #'
  #' @param an output file generated with a two-level model in Mplus,
  #' with intercepts and variables regressing on covariates.
  #'
  #' @return A vector with concatenated regression coefficients for the intercept, and the effects of age, sex and edu
  #' @export
  #'
  #' @examples
  #' extractbetacoefficients_mplus( mplusoutputfile = "twolevelunstructmod.out")

  modelparams <- extractModelParameters(target = mplusoutputfile)$unstandardized
  regressionmodelparams <- modelparams[ grepl(".ON", modelparams$paramHeader),]
  varnames <- modelparams$param[ modelparams$paramHeader == "Residual.Variances" & modelparams$BetweenWithin == "Within"]
  beta0 <- modelparams$est[ modelparams$paramHeader == "Means" & modelparams$BetweenWithin == "Between"]
  j <- 0
  k <- 1
  completecoefficients <- cbind( paste0(rep( varnames, each = 3), ".ON"), rep( covariates, length(varnames)), 0)
  for( i in 1:length(regressionmodelparams$est)){
    while( completecoefficients[k,1] != regressionmodelparams$paramHeader[i] | completecoefficients[k,2] != regressionmodelparams$param[i]){
      k <- k + 1
    }
    completecoefficients[k,3] <- regressionmodelparams$est[i]
    k <- k + 1
  }
  beta1 <- completecoefficients[grepl(covariates[1], completecoefficients[,2]),3]
  beta2 <- completecoefficients[grepl(covariates[2], completecoefficients[,2]),3]
  beta3 <- completecoefficients[grepl(covariates[3], completecoefficients[,2]),3]

  beta <- matrix(as.numeric(c(beta0, beta1, beta2, beta3)), ncol = 1)
  return(beta)
}




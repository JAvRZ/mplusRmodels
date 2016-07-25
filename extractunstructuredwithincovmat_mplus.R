library(MplusAutomation)


extractunstructuredwithincovmat_mplus <- function(mplusoutputfile){
  #' Construct a within unstructured covariance matrix from Mplus output
  #'
  #' @param an output file generated with a two-level model in Mplus,
  #' with a completely unstructured within covariance matrix.
  #'
  #' @return An unstructured covariance matrix with residual variances on the diagonal
  #' and covariances on the off-diagonal
  #' @export
  #'
  #' @examples
  #' extractwithincovmat_mplus( mplusoutputfile = "twolevelunstructmod.out")

modelparams <- extractModelParameters(target = mplusoutputfile)$unstandardized

varnames <- modelparams$param[ modelparams$paramHeader == "Residual.Variances" & modelparams$BetweenWithin == "Within"]
nvar <- length(varnames)

covmat <- matrix( NA, nvar, nvar)
colnames(covmat) <- rownames(covmat) <- varnames

modelcovparams <- modelparams[grepl( ".WITH", modelparams$paramHeader),]
modelcovparams[,1] <- gsub( ".WITH", "", modelcovparams[,1], fixed = TRUE)
for( i in 1:nrow(modelcovparams)){
covmat[rownames(covmat) == modelcovparams$paramHeader[ i ], colnames(covmat) == modelcovparams$param[ i ]] <- modelcovparams$est[ i ]
}
covmat[lower.tri(covmat)] <- t(covmat)[lower.tri(covmat)]
diag(covmat) <- modelparams$est[grepl( "Residual.Variances", modelparams$paramHeader)]

return(covmat)
}




library(MplusAutomation)


extractbetweencovmat_mplus <- function(mplusoutputfile){
  #' Construct a between diagonal covariance matrix from Mplus output
  #'
  #' @param an output file generated with a two-level model in Mplus,
  #' with a diagonal between covariance matrix.
  #'
  #' @return An diagonal covariance matrix with residual variances on the diagonal
  #' @export
  #'
  #' @examples
  #' extractbetweencovmat_mplus( mplusoutputfile = "twolevelunstructmod.out")

  modelparams <- extractModelParameters(target = mplusoutputfile)$unstandardized

  varnames <- modelparams$param[ modelparams$paramHeader == "Residual.Variances" & modelparams$BetweenWithin == "Within"]
  nvar <- length(varnames)

  covmat <- matrix( 0, nvar, nvar)
  diag(covmat) <- modelparams$est[grepl( "Between", modelparams$BetweenWithin) & grepl( "Variances", modelparams$paramHeader)]
  colnames(covmat) <- rownames(covmat) <- varnames
  return(covmat)
}




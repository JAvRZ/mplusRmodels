library(MplusAutomation)


extractfactorwithincovmat_mplus <- function(mplusoutputfile){
  #' Construct a within covariance matrix described by a factor structure from Mplus output
  #'
  #' @param an output file generated with a two-level model in Mplus,
  #' with a factor structured within covariance matrix.
  #'
  #' @return A factor structured within covariance matrix with residual variances on the diagonal
  #' and covariances on the off-diagonal
  #' @export
  #'
  #' @examples
  #' extractwithincovmat_mplus( mplusoutputfile = "twolevel2factmod.out")
  
modelparams <- extractModelParameters(target = mplusoutputfile)$unstandardized

varnames <- modelparams$param[ modelparams$paramHeader == "Residual.Variances" & modelparams$BetweenWithin == "Within"]
factnames <- modelparams$param[ modelparams$paramHeader == "Variances" & modelparams$BetweenWithin == "Within"]
nvar <- length(varnames)
nfact <- length(factnames)
psi <- matrix( 1, nfact, nfact)
if( nfact > 1){
psiraw <- modelparams[grepl( ".WITH", modelparams$paramHeader),]

for( i in 1:nrow(psiraw)){
psiraw[i,] <- gsub( ".WITH", "", psiraw[i,], fixed = TRUE)
psiraw$paramHeader[i] <- which( factnames == psiraw$paramHeader[i] )
psiraw$param[i] <- which( factnames == psiraw$param[i] )
}

for( i in 1:nrow(psiraw)){
psi[as.integer(psiraw$paramHeader[i]),as.integer(psiraw$param[i])] <-
  psi[as.integer(psiraw$param[i]),as.integer(psiraw$paramHeader[i])] <- 
    as.numeric(psiraw$est[i])
}
}


lambdaraw <- modelparams[grepl( ".BY", modelparams$paramHeader),]
for( i in 1:nrow(lambdaraw)){
lambdaraw$param[i] <- which( varnames == lambdaraw$param[i])
lambdaraw[i,] <- gsub( ".BY", "", lambdaraw[i,], fixed = TRUE)
lambdaraw$paramHeader[i] <- which( factnames == lambdaraw$paramHeader[i] )
}
lambda <- matrix( 0, nrow = nvar, ncol = nfact)
for( i in 1:nrow(lambdaraw)){
lambda[ as.integer(lambdaraw$param[i]), as.integer(lambdaraw$paramHeader[i])] <- as.numeric(lambdaraw$est[i])
}
theta <- diag(modelparams$est[grepl( "Residual.Variances", modelparams$paramHeader)])
covmat <- lambda  %*% psi %*% t(lambda) + theta

colnames(covmat) <- rownames(covmat) <- varnames
return(covmat)
}
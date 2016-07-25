mnc_mplus <- function( patientdata, Nnormpertest, beta, covmat, covariates = c("SEX","AGE", "EDU"), tails = 2){
  #' Calculate a MNC statistic from patient data, with regression coefficients and a
  #' covariance matrix estimated in a norm group.
  #'
  #' @param a patient data file containing demographic variables and test scores, 
  #' a vector of number of participants in the norm group for every variable,
  #' a vector of regression coefficients,
  #' a covariance matrix estimated in the norm group,
  #' a vector containing the names of the covariates,
  #' a value that indicates whether that tests are onesided with a lower expectation (-1), onesided with a higher expectation (1), or twosided (2).
  #'
  #' @return A list with a test statistic, degrees of freedom, and p-value.
  #' @export
  #'
  #' @examples
  #' MNC_mplus( patientdata = patdata, beta = mybeta, covmat = covbetween + covwithin)

  Tsquared <- c()
  pval <- c()
  df <- c()
  for( i in 1:nrow(patientdata)){
  completed <-  which( !is.na(patientdata[i,-(1:length(covariates))]))
  P <- length(completed)
  C <- covmat[completed,completed]
  invC <- solve(C)
  pred.y <- matrix(c( 1, as.numeric(patientdata[i,1:length(covariates)])), nrow = 1) %x% diag( 1, nrow(covmat)) %*% beta
  pred.y.completed <- pred.y[completed]

  est.n <- Nnormpertest[completed]
  min.est.n <- min(est.n)
  MNCdf <- min.est.n - P
  g <- ( min.est.n  + 1 ) / min.est.n

  patientscores <- as.numeric(patientdata[i,(length(covariates)+1) : length(patientdata[i,])][completed])
  Tsquared[i] <- ( 1 / g ) * ( ( min.est.n - P ) / ( ( min.est.n - 1 ) * P ) ) * t(pred.y.completed - patientscores ) %*% invC %*% ( pred.y.completed - patientscores )
  
  pval[i] <- pf( Tsquared[i], P, MNCdf, lower = FALSE)
  if( tails != 2){
    pval[i] <- pval[i]/2
    if( sign(sum(pred.y.completed - patientscores)) != tails){
      pval[i] <- 1
    }
  }
  df[i] <- paste( P, MNCdf ) 
  }
  return(list( Tsquared = Tsquared, df = df, pval = pval))
}

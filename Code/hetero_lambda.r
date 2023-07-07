#function that calculates the effect of hetereogenity in infection risk and returns the FOI 
hetero_lambda <- function(Bt,a,inf,total) {
  ## total population
  lambdahat <- Bt * exp(-a * inf/total) ## Infectious contact rate
  FOI <- lambdahat*(inf/total)
  #output <- cbind(FOI,lambdahat)
  return (FOI)
}

#function that calculates rthe effect of both hetereogenity in infection risk and behaviour and returns the FOI 
no_effect <- function(Bt,inf,total) {
  ## total population
  lambdahat <- Bt ## constant Infectious contact rate
  
  FOI <- lambdahat*(inf/total)
  return (FOI)
}
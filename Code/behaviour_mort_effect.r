#function that calculates the effect of behaviour  and returns the FOI 
behaviour_mort_effect <- function(Bt,a,inf,total,g,I4,q) {
  ## total population
  lambdahat <- Bt  ## Infectious contact rate
  mortResponse <- exp(-q*g*I4/total) # effect of behaviour on FOI
  FOI <- mortResponse*lambdahat*(inf/total)
  return (FOI)
}
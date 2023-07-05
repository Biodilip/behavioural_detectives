#function that calculates rthe effect of both hetereogenity in infection risk and behaviour and returns the FOI 
both_effects <- function(Bt,a,inf,total,g,I4,q) {
  ## total population
  lambdahat <- Bt * exp(-a * inf/total) ## Infectious contact rate
  mortResponse <- exp(-q*g*I4/total) # effect of behaviour on FOI
  FOI <- mortResponse*lambdahat*(inf/total)
  return (FOI)
}
#function that calculates the effect of behaviour  and returns the FOI 
behaviour_mort_effect <- function(Bt,a,inf,total,g,I4,q) {
  ## total population
  lambdahat <- Bt  ## Infectious contact rate
  mortResponse <- exp(-q*g*I4/total) # effect of behaviour on FOI
  FOI <- mortResponse*lambdahat*(inf/total)
  #output <- cbind(FOI,lambdahat)
  return (FOI)
  #return (output)
}
# 
# g <- (1/10)*1
# q <- 10
# k<-1
# Bt <- 1.1
# a  = 8
# inf = SI.ts[,"I"]
# total = SI.ts[,"N"]
# I4 = SI.ts[,k+2]
# 
# test_behav<-behaviour_mort_effect(Bt,a,inf,total,g,I4,q)
# 
# plot(SI.ts$P, test_behav$I,'l')
# 
# plot(SI.ts$P, test_behav$N,'l')
# 

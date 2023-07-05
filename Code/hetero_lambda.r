#function that calculates the effect of hetereogenity in infection risk and returns the FOI 
hetero_lambda <- function(Bt,a,inf,total) {
  ## total population
  lambdahat <- Bt * exp(-a * inf/total) ## Infectious contact rate
  FOI <- lambdahat*(inf/total)
  output <- cbind(FOI,lambdahat)
  return (output)
}
# Bt <- 1.1
# a  = 4
# inf = SI.ts[,"I"]
# total = SI.ts[,"N"]
# 
# test_het <-hetero_lambda(Bt,a,inf,total)
# 
# plot(SI.ts$P, test_het$I,'l')
# 
# plot(SI.ts$P, test_het$N,'l')
# 
# plot(test_het$I)
# 

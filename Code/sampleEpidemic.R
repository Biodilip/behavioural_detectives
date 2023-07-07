

# source simEpidemic
source("./Code/simEpidemic.R")

## Function to 'sample' the population:
## From a simulated epidemic, measure prevalence at several time points by drawing
## cross-sectional samples of individuals at each time, testing them, and then calculating sample
## prevalence and associated binomial confidence intervals

sampleEpidemic <- function(simDat # Simulated "data" which we treat as real 
                           , sampleDates = seq(1, max(time.out), by = 365*3) # Sample every 3 years 
                           , numSamp = rep(100, length(sampleDates)) # Number of individuals sampled at each time point
){
  prev_at_sample_times <- simDat[simDat$time %in% sampleDates, 'P']
  if (length(prev_at_sample_times) != length(numSamp)) {
    stop("Number of samples not equal to length of requested samples")
  }
  numPos <- rbinom(length(numSamp), numSamp, prev_at_sample_times)
  # ?change to beta binomial rbeta(n, shape1, shape2, ncp = 0)
  # ?sample subset of population
  lci <- mapply(function(x,n) binom.test(x,n)$conf.int[1], x = numPos, n = numSamp)
  uci <- mapply(function(x,n) binom.test(x,n)$conf.int[2], x = numPos, n = numSamp)    
  return(data.frame(time = sampleDates, numPos, numSamp, sampPrev =  numPos/numSamp,
                    lci = lci, uci = uci))
}

## Run system of ODEs for "true" parameter values
trueParms <- disease_params() # Default model parameters are defined in lines 20-26
simDat <- simEpidemic(pop.SI0, parms = trueParms) # Simulated epidemic (underlying process)

# Simulate data from the sampling process (function defined above)
#myDat <- sampleEpidemic(simDat) 


list_of_myDat <- list()
count= 1
for (freq in c(1,10, 20)){
  out<-sampleEpidemic(simDat, sampleDates = seq(1, max(time.out), by = 365*freq))
  list_of_myDat[[count]] <- out
  count =count +1
}





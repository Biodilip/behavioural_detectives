
# libraries
require(boot); require(deSolve); require(ellipse);

# source script
source("./Code/behaviouralODEs.r")

# Sampling frequency
seqMonth <- seq(1,max(time.out), by = 12)

# Number of Is based on number of selected box cars
Is <- paste0('I',1:k) ## for easy indexing

# into months
tseqMonth <- seq(1,max(time.out), by = 12)


## Function to run the deterministic model simulation, based on the ODE system defined in HIV_SI().
simEpidemic <- function(init, tseq = tseqMonth, modFunction=HIV_SI_ODEs, parms = disease_params()) {
  simDat <- as.data.frame(lsoda(init, tseq, modFunction, parms=parms))
  #simDat$I <- rowSums(simDat[, Is])
  #simDat$N <- rowSums(simDat[, c('S',Is)])
  #simDat$P <- with(simDat, I/N)
  return(simDat)
}


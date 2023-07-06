# Load libraries
library(deSolve)                # Load library to be used for numerical integration
library(data.table)
library(ggplot2)
library(GillespieSSA)
rm(list=ls())                   # Clear all variables and functions
# Sources
source("popmod.R")

# Population
N0=500000

# Function for SI


lambdahat <- Beta * exp(-alpha * inf/total) ## Infectious contact rate
mortResponse <- exp(-q*pRT*tail(I_vec,n=1)/total) # effect of behaviour on FOI
FOI <- mortResponse*lambdahat*(inf/total)
matrix(c(-1,0,+1,-1,0,+1),nrow=3,byrow=TRUE)

## Kermack-McKendrick SIR model
x0 <- c(S=5000,I=0)
c(disease_params(),S=0,I=11)->sim.parms
a <- c("Beta*exp(-alpha*I/(I+S))*exp(-q*pRt*tail(I_vec,1)/(I+S))*I/(I+S)",paste("pRt*I"))
nu <- matrix(c(-1,0,+1,-1),nrow=2,byrow=TRUE)
out <- ssa(x0,a,nu,sim.parms,tf=100,method=ssa.d(),simName="SIR model")
ssa.plot(out)

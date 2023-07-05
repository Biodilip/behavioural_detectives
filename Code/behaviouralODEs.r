## Introduction to Infectious Disease Dynamics
## Clinic on the Meaningful Modeling of Epidemiological Data
## International Clinics on Infectious Disease Dynamics and Data (ICI3D) Program
## African Institute for Mathematical Sciences, Muizenberg, RSA
##
## Heterogeneity or behaviour modification - Can we tell?
## Sam Abbot, ......
##



library(deSolve)                # Load library to be used for numerical integration
library(data.table)
library(ggplot2)



rm(list=ls())                   # Clear all variables and functions
## Our state variables
## 
## S(t) - The number of susceptible adults over the age of 25 years at time t
## I(t) - The number of infected adults over the age of 25 years at time t
## with N(t) = S(t) + I(t)

## and parameters:
##
## N - the total population size; N = S + I [inidividuals]
## birthRt - per capita birth rate [1 / time]
## lambda - per capita effective rate
## delta - disease induced mortality hazard
## deathRt - per capita mortality rate
## Beta - transmission coefficient when prevalence is 0 
##alpha - for transmission coefficient: decline with prevalence


##-- HIV - SI model from --Hargrove, John W., et al. 
#"Declining HIV prevalence and incidence in perinatal women in Harare, Zimbabwe." Epidemics 3.2 (2011): 88-94.
HIV_SI <- function(t,y,parms){
  with(c(as.list(y),parms),{
    #N <- sum(y)
    
    I <- I1+I2+I3+I4           ## total infected
    N <- I + S                 ## total population
    #lambda <- hetero_lambda(Beta,alpha,I,N)
    #lambda <- behaviour_mort_effect(Beta,alpha,I,N,n,I4,q)
    lambda <- both_effects(Beta,alpha,I,N,n,I4,q)
    ## state variable derivatives (ODE system)
    deriv <- rep(NA,7)
    deriv[1] <-	bRt*N - (lambda+dRt)*S  ## Instantaneous rate of change: Susceptibles
    deriv[2] <-	(lambda+dRt)*S - pRt*I1 - dRt*I1 ## Instantaneous rate of change: Infection class I1
    deriv[3] <-	pRt*I1 - pRt*I2 - dRt*I2 ## Instantaneous rate of change:  Infection class I2
    deriv[4] <-	pRt*I2 - pRt*I3 - dRt*I3 ## Instantaneous rate of change: Infection class I3 
    deriv[5] <-	pRt*I3 - pRt*I4 - dRt*I4 ## Instantaneous rate of change: Infection class I4
    deriv[6] <-	(lambda)*S ## Instantaneous rate of change: Cumulative incidence
    deriv[7] <-	pRt*I4 ## Instantaneous rate of change: Cumulative mortality
    return(list(deriv))
  })
}
#function that calculates the effect of hetereogenity in infection risk and returns the FOI 
hetero_lambda <- function(Bt,a,inf,total) {
                 ## total population
    lambdahat <- Bt * exp(-a * inf/total) ## Infectious contact rate
    FOI <- lambdahat*(inf/total)
  return (FOI)
}

#function that calculates the effect of behaviour  and returns the FOI 
behaviour_mort_effect <- function(Bt,a,inf,total,g,I4,q) {
  ## total population
  lambdahat <- Bt  ## Infectious contact rate
  mortResponse <- exp(-q*g*I4/total) # effect of behaviour on FOI
  FOI <- mortResponse*lambdahat*(inf/total)
  return (FOI)
}

#function that calculates rthe effect of both hetereogenity in infection risk and behaviour and returns the FOI 
both_effects <- function(Bt,a,inf,total,g,I4,q) {
  ## total population
  lambdahat <- Bt * exp(-a * inf/total) ## Infectious contact rate
  mortResponse <- exp(-q*g*I4/total) # effect of behaviour on FOI
  FOI <- mortResponse*lambdahat*(inf/total)
  return (FOI)
}

## Function that makes a list of disease parameters with default values
disease_params <- function(Beta = 0.9 ## transmission coefficient when prevalence is 0 
                           , alpha = 8 ## for transmission coefficient: decline with prevalence
                           , n = 4 # number of box cars
                           , pRt = (1/10)*n ## rate of of progression through each of the I classes, for 10 years total
                           , bRt = .03 ## birth rate, 3% of people give birth per year
                           , dRt = 1/60 ## 60 year natural life expectancy
                           , q = 50 # effect of behaviour change on mortality
){
  return(as.list(environment()))
}

disease_params()
#Initial conditions
initPrev <- exp(-7) ## infected at start
pop.SI0 <- c(S=1-initPrev, 
             I1=initPrev, 
             I2=0, I3=0, 
             I4=0, CI = 0, 
             CD = 0)      #Initial conditions
time.out <- seq(0, 365, 1)

## Solve ODE using lsoda()...
model1_ts <- data.table(lsoda(
  y = pop.SI0,               # Initial conditions for population
  times = time.out,             # Timepoints for evaluation
  func = HIV_SI,                   # Function to evaluate
  parms = disease_params()               # Vector of parameters
))

#format solutions to have I, N and P
model1_ts[, I := I1 + I2 + I3 + I4]
model1_ts[, N := S+I]
model1_ts[, P := I / N]

#plot output
with(model1_ts, {
  # plotting the time series of susceptibles:
  plot(time, S, type = "l", col = "blue",
       xlab = "time (days)", ylab = "number of people")
  # adding the time series of infectious:
  lines(time, I, col = "red")
  # adding the time series of recovered:
  #lines(time, col = "green")
})


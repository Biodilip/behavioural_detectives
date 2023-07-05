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
## N - the total population size; N = S + I [individuals]
## bRt - per capita birth rate [1 / time]
## dRt - per capita mortality rate
## Beta - transmission coefficient when prevalence is 0 
##alpha - for transmission coefficient: decline with prevalence


##-- HIV - SI model from --Hargrove, John W., et al. 
#"Declining HIV prevalence and incidence in perinatal women in Harare, Zimbabwe." Epidemics 3.2 (2011): 88-94.
HIV_SI <- function(t,y,parms,n){
  with(c(as.list(y),parms),{
    #N <- sum(y)
    
    I_vec <- y[2:(n+1)]
    I <-  sum(I_vec)          ## total infected
    N <- I + S                 ## total population
    #labda <- hetero_lambda(Beta,alpha,I,N)
    #lambda <- behaviour_mort_effect(Beta,alpha,I,N,n,I4,q)
    lambda <- both_effects(Beta,alpha,I,N,n,I4,q)
    ## state variable derivatives (ODE system)
    
    deriv <- rep(NA,3+(n)) # number of derivatives
    deriv[1] <-	bRt*N - (lambda+dRt)*S  ## Instantaneous rate of change: Susceptibles
    #deriv[2] <-	(lambda+dRt)*S - pRt*I1 - dRt*I1 ## Instantaneous rate of change: Infection class I1
    #deriv[3] <-	pRt*I1 - pRt*I2 - dRt*I2 ## Instantaneous rate of change:  Infection class I2
    #deriv[4] <-	pRt*I2 - pRt*I3 - dRt*I3 ## Instantaneous rate of change: Infection class I3 
    #deriv[5] <-	pRt*I3 - pRt*I4 - dRt*I4 ## Instantaneous rate of change: Infection class I4
    deriv[2:(n+1)] <- box_car_infections(S, I_vec,lambda, dRt,pRt)
    deriv[(length(deriv)-1)] <-	(lambda)*S ## Instantaneous rate of change: Cumulative incidence
    deriv[length(deriv)] <-	pRt*deriv[n+1] ## Instantaneous rate of change: Cumulative mortality
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
#function

box_car_infections <- function(S, I_vec,lambda, dRt,pRt) {
  n <- length(I_vec)
  dI_vec <- numeric(n)
  dI_vec[1] <- (lambda)*S - pRt * I_vec[1] - dRt * I_vec[1]
  if (n > 1 ) {
    for (i in 2:n) {
      dI_vec[i] <- pRt * I_vec[i-1] - pRt * I_vec[i] - dRt * I_vec[i] 
    }
  }
  return(dI_vec)
}




## Function that makes a list of disease parameters with default values
disease_params <- function(Beta = 0.9 ## transmission coefficient when prevalence is 0 
                           , alpha = 8 ## for transmission coefficient: decline with prevalence
                           , n = 4 ## number of box  cars
                           , pRt = (1/10)*n ## rate of of progression through each of the I classes, for 10 years total
                           , bRt = .03 ## birth rate, 3% of people give birth per year
                           , dRt = 1/60 ## 60 year natural life expectancy
                           , q = 50 # effect of behaviour change on mortality
){
  return(as.list(environment()))
}


#Initial conditions
initPrev <- exp(-7) ## infected at start
k = disease_params()$n
I_vec <- rep(0,(k-1))
names(I_vec) <- paste0("I",2:k)
pop.SI0 <- c(S=1-initPrev, 
             I1=initPrev, 
             I_vec, CI = 0, 
             CD = 0)      #Initial conditions
time.out <- seq(0, 365, 1)




# Set name to Vector


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


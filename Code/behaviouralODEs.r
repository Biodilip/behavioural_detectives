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

setwd("C:/Users/corlendo/behavioural_detectives")
source("code/hetero_lambda.r")
source("code/behaviour_mort_effect.r")
source("code/both_effects.r")
source("code/box_car_infections.r")
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

#Disease parameters
## Function that makes a list of disease parameters with default values
disease_params <- function(Beta = 0.9 ## transmission coefficient when prevalence is 0 
                           , alpha = 8 ## for transmission coefficient: decline with prevalence
                           , n = 2 ## number of box  cars
                           , pRt = (1/10)*n ## rate of of progression through each of the I classes, for 10 years total
                           , bRt = 1/60 ## birth rate = death rates in a closed popn
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


##-- HIV - SI model from --Hargrove, John W., et al. 
#"Declining HIV prevalence and incidence in perinatal women in Harare, Zimbabwe." Epidemics 3.2 (2011): 88-94.
HIV_SI <- function(t,y,parms,n){
  with(c(as.list(y),parms),{
    #N <- sum(y)
    
    I_vec <- y[2:(n+1)]
    I <-  sum(I_vec)          ## total infected
    N <- I + S                 ## total population
    lambda <- hetero_lambda(Beta,alpha,I,N)
    #lambda <- behaviour_mort_effect(Beta,alpha,I,N,n,tail(I_vec,n=1),q)
    #lambda <- both_effects(Beta,alpha,I,N,n,tail(I_vec,n=1),q)
    ## state variable derivatives (ODE system)
    
    deriv <- rep(NA,3+(n)) # number of derivatives
    deriv[1] <-	bRt*N - (lambda+dRt)*S  ## Instantaneous rate of change: Susceptibles
    deriv[2:(n+1)] <- box_car_infections(S, I_vec,lambda, dRt,pRt)
    deriv[(length(deriv)-1)] <-	(lambda)*S ## Instantaneous rate of change: Cumulative incidence
    deriv[length(deriv)] <-	pRt*deriv[n+1] ## Instantaneous rate of change: Cumulative mortality
    return(list(deriv))
  })
}


## Solve ODE using lsoda and give our output in the data.table
SI.ts <- data.table(lsoda(
  y = pop.SI0,               # Initial conditions for population
  times = time.out,             # Timepoints for evaluation
  func = HIV_SI,                   # Function to evaluate
  parms = disease_params()               # Vector of parameters
))



SI.ts$I <- rowSums(SI.ts[,3:(k+2)])
#SI.ts <- SI.ts[,-3:-(k+2)]
SI.ts$N <- rowSums(SI.ts[,3:ncol(SI.ts)])
SI.ts[, P := I / N]

#plot output
SI.ts.long <- melt(SI.ts, id.vars = 'time')

(ggplot(SI.ts.long)
  + aes(x = time, y = value, color = variable, linetype = variable)
  + geom_line()
)

(ggplot(SI.ts.long[variable %in% c('S', 'I', 'CD')])
  + aes(x = time, y = value)
  + geom_line()
  + facet_wrap(~ variable)
)


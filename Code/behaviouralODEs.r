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

<<<<<<< HEAD
setwd("C:/Users/corlendo/behavioural_detectives")
source("code/hetero_lambda.r")
source("code/behaviour_mort_effect.r")
source("code/both_effects.r")
source("code/box_car_infections.r")
source("code/HIV_SI_ODEs.r")

=======
source("hetero_lambda.r")
source("behaviour_mort_effect.r")
source("both_effects.r")
source("box_car_infections.r")
>>>>>>> 66c355e48fe982936e9c866e2c41aa3412a3949b
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
                           , n = 6 ## number of box  cars
                           , pRt = (1/10)*n ## rate of of progression through each of the I classes, for 10 years total
                           , bRt = 1/60 ## birth rate = death rates in a closed popn
                           , dRt = 1/60 ## 60 year natural life expectancy
                           , q = 150 # effect of behaviour change on mortality
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

(ggplot(SI.ts.long[variable %in% c('S', 'I', "CI","CD","N")])
  + aes(x = time, y = value)
  + geom_line()
  + facet_wrap(~ variable)
)


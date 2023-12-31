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
source("Code/hetero_lambda.r")
source("Code/no_effect.r")
source("Code/behaviour_mort_effect.r")
source("Code/both_effects.r")
source("Code/box_car_infections.r")
source("Code/HIV_SI_ODEs.r")


## Script that explores the effect of heterogeneity or behaviour on model 

#1) Define the disease parameters
## Function that makes a list of disease parameters with default values
disease_params <- function(Beta = 0.2/(30) ## transmission coefficient when prevalence is 0 
                           , alpha = 4.5 ## for transmission coefficient: decline with prevalence
                           , n = 1 ## number of box  cars
                           , daysYear = 365 ## no of days in a year
                           , pRt = (1/(10*daysYear))*n ## rate of of progression through each of the I classes, for 10 years total
                           , bRt = 1/(60*daysYear) ## birth rate = death rates in a closed popn
                           , dRt = 1/(60*daysYear) ## 60 year natural life expectancy
                           , q =10 # effect of behaviour change on mortality
                           , effect = 'none'
){
  return(as.list(environment()))
}

#Initial conditions
initPrev <- exp(-7) ## infected at start
k = disease_params()$n
I_vec <- rep(0,(k))
I_vec[1] <- initPrev
names(I_vec) <- paste0("I",1:k)
pop.SI0 <- c(S=1-initPrev, 
             I_vec, CI = 0, 
             CD = 0)      #Initial conditions
noyears <- 40
time.out <- seq(0, 365*noyears, 1)



## Solve ODE using lsoda and give our output in the data.table
SI.ts <- data.table(lsoda(
   y = pop.SI0,               # Initial conditions for population
   times = time.out,             # Timepoints for evaluation
   func = HIV_SI_ODEs,                   # Function to evaluate
   parms = disease_params()
 ))

SI.ts$I <- rowSums(SI.ts[,3:(k+2)])
SI.ts <- SI.ts[,-3:-(k+2)]
SI.ts$N <- SI.ts[,I] + SI.ts[,S]
SI.ts[, P := I / N]
SI.ts[, CIR := CI / N]
SI.ts[, CDR := CD / N]




#plot output
SI.ts.long <- melt(SI.ts, id.vars = 'time')

(ggplot(SI.ts.long[variable %in% c('S', 'I', "P","CDR","CD","N")])
  + aes(x = time, y = value)
  + geom_line()
  + facet_wrap(~ variable,scales = "free")
)


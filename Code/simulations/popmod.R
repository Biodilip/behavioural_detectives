# Load libraries
library(deSolve)                # Load library to be used for numerical integration
library(data.table)
library(ggplot2)
rm(list=ls())                   # Clear all variables and functions

# Sources
source("../hetero_lambda.r")
source("../behaviour_mort_effect.r")
source("../both_effects.r")
source("../box_car_infections.r")
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
disease_params <- function(Beta = 0.2 ## transmission coefficient when prevalence is 0 
                           , alpha = 4.5 ## for transmission coefficient: decline with prevalence
                           , n = 4 ## number of box cars
                           , pRt = (1/(10))*n ## rate of of progression through each of the I classes, for 10 years total
                           , bRt = 0.029## birth rate
                           , dRt = 1/60## 60 year natural life expectancy
                           , q = 10 # effect of behaviour change on mortality
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
years=10
time.out <- seq(0, 365*years, 1)


##-- HIV - SI model from --Hargrove, John W., et al. - to include variable number of boxcars
#"Declining HIV prevalence and incidence in perinatal women in Harare, Zimbabwe." Epidemics 3.2 (2011): 88-94.
HIV_SI <- function(t,y,parms,n){
  with(c(as.list(y),parms),{
    #N <- sum(y)
    I_vec <- y[2:(n+1)]
    I <-  sum(I_vec)          ## total infected
    N <- I + S                 ## total population
    #lambda<-Beta*I/N
    #lambda <- hetero_lambda(Beta,alpha,I,N)[1]
    #lambda <- behaviour_mort_effect(Beta,alpha,I,N,n,tail(I_vec,n=1),q)[1]
    lambda <- both_effects(Beta,alpha,I,N,n,tail(I_vec,n=1),q)
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

# Make data.frame of time steps
SI.ts$I <- rowSums(SI.ts[,3:(k+2)])
# SI.ts <- SI.ts[,-3:-(k+2)]
SI.ts$N <- rowSums(SI.ts[,2:ncol(SI.ts)])
SI.ts[, P := I / N]
SI.ts[, CDR := CD / N]
SI.ts[, CIR := CI / N]

#plot output
SI.ts.long <- melt(SI.ts, id.vars = 'time')
#par(mar = c(5, 4, 4, 4) + 0.3)
par(mfrow=c(1,2))
with(SI.ts.long[variable=="P",],
	plot(time,value,type="l",col="green"))
#par(new=T)
#with(SI.ts.long[variable=="CIR",],
#	plot(time,value,type="l",col="blue",axes=F))
#with(SI.ts.long[variable=="CDR",],
#	lines(time,value,type="l",col="red"))
#axis(side = 4, at = seq(0,0.09,by=0.01))
#mtext("CI/CD", side = 4, line = 3)
with(SI.ts.long[variable=="CIR",],
	plot(time,value,type="l",col="blue"))
with(SI.ts.long[variable=="CDR",],
	lines(time,value,type="l",col="red"))
legend("topright",c("P","CIR","CDR"), col=c("green","blue","red"),lty=1)

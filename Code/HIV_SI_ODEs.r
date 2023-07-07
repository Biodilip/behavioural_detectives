## Our state variables
## 
## S(t) - The number of susceptible adults over the age of 25 years at time t
## I(t) - The number of infected adults over the age of 25 years at time t
## with N(t) = S(t) + I(t)

##-- HIV - SI model from --Hargrove, John W., et al. 
#"Declining HIV prevalence and incidence in perinatal women in Harare, Zimbabwe." Epidemics 3.2 (2011): 88-94.
HIV_SI_ODEs <- function(t,y,parms){
  with(c(as.list(y),parms),{
    #N <- sum(y)
    I_vec <- y[2:(n+1)]
    I <-  sum(I_vec)          ## total infected
    N <- I + S                 ## total population
    
  
    if (effect=="none") {
       lambda <- no_effect(Beta,I,N)
      } else if (effect=="heterogeneity") {
        lambda <- hetero_lambda(Beta,alpha,I,N)
      } else if (effect=="behaviour") {
        lambda <- behaviour_mort_effect(Beta,alpha,I,N,n,tail(I_vec,n=1),q)
      } else if  (effect=="both") {
        lambda <- both_effects(Beta,alpha,I,N,n,tail(I_vec,n=1),q)
      }
     
    
    deriv <- rep(NA,3+(n)) # number of derivatives
    deriv[1] <-	bRt*N - lambda*S - dRt*S  ## Instantaneous rate of change: Susceptibles
    deriv[2:(n+1)] <- box_car_infections(S, I_vec,lambda, dRt,pRt) ##Instantenous rate of change: Infectious
    deriv[(length(deriv)-1)] <-	(lambda)*S ## Instantaneous rate of change: Cumulative incidence
    deriv[length(deriv)] <-	pRt*deriv[n+1] ## Instantaneous rate of change: Cumulative mortality
    return(list(deriv))
  })
}


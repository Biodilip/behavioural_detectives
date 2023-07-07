
## Heterogeneity or behaviour modification - Can we tell? to explore many settings
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
                           , n = 4 ## number of box  cars
                           , daysYear = 365 ## no of days in a year
                           , pRt = (1/(10*daysYear))*n ## rate of of progression through each of the I classes, for 10 years total
                           , bRt = 1/(60*daysYear) ## birth rate = death rates in a closed popn
                           , dRt = 1/(60*daysYear) ## 60 year natural life expectancy
                           , q = 10 # effect of behaviour change on mortality
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


#create a dataframe of varying inputs for the effects beign explored
#base case parameters
no_effect_parms_base <- disease_params(Beta = 0.2/(30), alpha = 4.5, 
                                       n = 4 , q = 10, effect = 'none')
hetero_effect_parms_base <- disease_params(Beta = 0.2/(30), alpha = 4.5, 
                                           n = 4 , q = 10, effect = 'heterogeneity')
behaviour_effect_parms_base <- disease_params(Beta = 0.2/(30), alpha = 4.5, 
                                              n = 4 , q = 10, effect = 'behaviour')
both_effects_parms_base <- disease_params(Beta = 0.2/(30), alpha = 4.5, 
                                          n = 4 , q = 10, effect = 'both')

#beta half
no_effect_parms_beta_half <- disease_params(Beta = 0.2/(30*2), alpha = 4.5, 
                                            n = 4 , q = 10, effect = 'none')
hetero_effect_parms_beta_half <- disease_params(Beta = 0.2/(30*2), alpha = 4.5, 
                                                n = 4 , q = 10, effect = 'heterogeneity')
behaviour_effect_parms_beta_half <- disease_params(Beta = 0.2/(30*2), alpha = 4.5, 
                                                   n = 4 , q = 10, effect = 'behaviour')
both_effects_parms_beta_half <- disease_params(Beta = 0.2/(30*2), alpha = 4.5, 
                                               n = 4 , q = 10, effect = 'both')

#alpha 2
no_effect_parms_alpha_2 <- disease_params(Beta = 0.2/(30), alpha = 2.5, 
                                          n = 4 , q = 10, effect = 'none')
hetero_effect_parms_alpha_2 <- disease_params(Beta = 0.2/(30), alpha = 2.5, 
                                              n = 4 , q = 10, effect = 'heterogeneity')
behaviour_effect_parms_alpha_2 <- disease_params(Beta = 0.2/(30), alpha = 2.5, 
                                                 n = 4 , q = 10, effect = 'behaviour')
both_effects_parms_alpha_2 <- disease_params(Beta = 0.2/(30), alpha = 2.5, 
                                             n = 4 , q = 10, effect = 'both')

# q =  50
no_effect_parms_q_50 <- disease_params(Beta = 0.2/(30), alpha = 4.5, 
                                       n = 4 , q = 50, effect = 'none')
hetero_effect_parms_q_50 <- disease_params(Beta = 0.2/(30), alpha = 4.5, 
                                           n = 4 , q = 50, effect = 'heterogeneity')
behaviour_effect_parms_q_50 <- disease_params(Beta = 0.2/(30), alpha = 4.5, 
                                              n = 4 , q = 50, effect = 'behaviour')
both_effects_parms_q_50 <- disease_params(Beta = 0.2/(30), alpha = 4.5, 
                                          n = 4 , q = 50, effect = 'both')


# 1 box car
no_effect_parms_1_box <- disease_params(Beta = 0.2/(30), alpha = 4.5, 
                                        n = 1 , q = 10, effect = 'none')
hetero_effect_parms_1_box <- disease_params(Beta = 0.2/(30), alpha = 4.5, 
                                            n = 1 , q = 10, effect = 'heterogeneity')
behaviour_effect_parms_1_box <- disease_params(Beta = 0.2/(30), alpha = 4.5, 
                                               n = 1 , q = 10, effect = 'behaviour')
both_effects_parms_1_box <- disease_params(Beta = 0.2/(30), alpha = 4.5, 
                                           n = 1 , q = 10, effect = 'both')



list_effects <- list(no_effect_parms_base,hetero_effect_parms_base,
                     behaviour_effect_parms_base,both_effects_parms_base,
                     no_effect_parms_beta_half,hetero_effect_parms_beta_half,
                     behaviour_effect_parms_beta_half,both_effects_parms_beta_half,
                     no_effect_parms_alpha_2,hetero_effect_parms_alpha_2,
                     behaviour_effect_parms_alpha_2,both_effects_parms_alpha_2,
                     no_effect_parms_q_50,hetero_effect_parms_q_50,
                     behaviour_effect_parms_q_50,both_effects_parms_q_50)

#list_effects <- list(no_effect_parms_1_box)
#no_effect_parms_1_box,hetero_effect_parms_1_box,
#behaviour_effect_parms_1_box,both_effects_parms_1_box,
#no_effect_parms_q_50,hetero_effect_parms_q_50,
#behaviour_effect_parms_q_50,both_effects_parms_q_50
mylist <- list() #create an empty list
file.list <- c()
for (i in 1:16) {
  mylist[[i]] <- data.table(lsoda(
    y = pop.SI0,               # Initial conditions for population
    times = time.out,             # Timepoints for evaluation
    func = HIV_SI_ODEs,                   # Function to evaluate
    parms =list_effects[[i]]
  ))
  mylist[[i]]$I <- rowSums( mylist[[i]][,3:(k+2)])
  mylist[[i]]$N <- mylist[[i]][,I] + mylist[[i]][,S]
  mylist[[i]][, P := I / N]
  mylist[[i]][, CIR := CI / N]
  mylist[[i]][, CDR := CD / N]
  filename<- paste("effect",list_effects[[i]]$effect,"_Beta",
                   list_effects[[i]]$Beta,"_alpha",list_effects[[i]]$alpha,
                   "_n",list_effects[[i]]$n,"_q",list_effects[[i]]$q,sep="")
  a<- mylist[[i]]
  saveRDS(a, file=paste("Results/",filename,sep="",".rds"))
  file.list[i] <- paste0(filename,".rds")
}

saveRDS(file.list, file ="Results/file_list.rds")





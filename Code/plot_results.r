## Introduction to Infectious Disease Dynamics
## Clinic on the Meaningful Modeling of Epidemiological Data
## International Clinics on Infectious Disease Dynamics and Data (ICI3D) Program
## African Institute for Mathematical Sciences, Muizenberg, RSA
##
## Heterogeneity or behaviour modification - Can we tell?
## Sam Abbot, ......
##



#library(deSolve)                # Load library to be used for numerical integration
library(data.table)
library(ggplot2)



rm(list=ls())                   # Clear all variables and functions



#setwd("C:/Users/corlendo/behavioural_detectives/base_parms_objects")
## Script that plots our results 

#import data files


no_effect_q_0_alpha_0_n_1 <- load(file = "no_effect_q_0_alpha_0_n_1.rda")
no_effect_q_0_alpha_0_n_4 <- load(file = "no_effect_q_0_alpha_0_n_4.rda")
hetero_effect_q_0_alpha_45_n_1.rda <- load(file = "hetero_effect_q_0_alpha_45_n_1.rda")
hetero_effect_q_0_alpha_45_n_4.rda <- load(file = "hetero_effect_q_0_alpha_45_n_4.rda")
behaviour_effect_q_4_alpha_45_n_1.rda <- load(file = "behaviour_effect_q_4_alpha_45_n_1.rda")
behaviour_effect_q_4_alpha_45_n_4.rda <- load(file = "behaviour_effect_q_4_alpha_45_n_4.rda")
behaviour_effect_q_10_alpha_45_n_1.rda <- load(file = "behaviour_effect_q_10_alpha_45_n_1.rda")
behaviour_effect_q_10_alpha_45_n_4.rda <- load(file = "behaviour_effect_q_10_alpha_45_n_4.rda")
both_effect_q_4_alpha_45_n_1.rda <- load(file="both_effect_q_4_alpha_45_n_1.rda")
both_effect_q_4_alpha_45_n_4.rda <- load(file="both_effect_q_4_alpha_45_n_4.rda")
both_effect_q_10_alpha_45_n_1.rda <- load(file="both_effect_q_10_alpha_45_n_1.rda")
both_effect_q_10_alpha_45_n_4.rda <- load(file="both_effect_q_10_alpha_45_n_4.rda")






(ggplot(SI.ts.long[variable %in% c('S', 'I', "P","CDR","CD","N")])
  + aes(x = time, y = value)
  + geom_line()
  + facet_wrap(~ variable,scales = "free")
)


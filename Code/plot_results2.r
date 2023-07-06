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
library(dplyr)
library(viridis)



rm(list=ls())                   # Clear all variables and functions



setwd("C:/Users/corlendo/behavioural_detectives/base_parms_objects")
## Script that plots our results 

#import data files


#no_effect_q_0_alpha_0_n_1 <- load(file = "no_effect_q_0_alpha_0_n_1.rda")
no_effect_q_0_alpha_0_n_4 <- load(file = "no_effect_q_0_alpha_0_n_4.rda")
no_effect_q_0_alpha_0_n_4_beta_half <- load(file = "no_effect_q_0_alpha_0_n_4_beta_half.rda")
#hetero_effect_q_0_alpha_45_n_1.rda <- load(file = "hetero_effect_q_0_alpha_45_n_1.rda")
hetero_effect_q_10_alpha_45_n_4.rda <- load(file = "hetero_effect_q_0_alpha_45_n_4.rda")
hetero_effect_q_10_alpha_45_n_4_beta_half.rda <- load(file = "hetero_effect_q_10_alpha_45_n_4_beta_half.rda")
#behaviour_effect_q_4_alpha_45_n_1.rda <- load(file = "behaviour_effect_q_4_alpha_45_n_1.rda")
#behaviour_effect_q_4_alpha_45_n_4.rda <- load(file = "behaviour_effect_q_4_alpha_45_n_4.rda")
#behaviour_effect_q_10_alpha_45_n_1.rda <- load(file = "behaviour_effect_q_10_alpha_45_n_1.rda")
behaviour_effect_q_10_alpha_45_n_4.rda <- load(file = "behaviour_effect_q_10_alpha_45_n_4.rda")
behaviour_effect_q_10_alpha_45_n_4_beta_half.rda <- load(file = "behaviour_effect_q_10_alpha_45_n_4_beta_half.rda")
#both_effect_q_4_alpha_45_n_1.rda <- load(file="both_effect_q_4_alpha_45_n_1.rda")
#both_effect_q_4_alpha_45_n_4.rda <- load(file="both_effect_q_4_alpha_45_n_4.rda")
#both_effect_q_10_alpha_45_n_1.rda <- load(file="both_effect_q_10_alpha_45_n_1.rda")
both_effect_q_10_alpha_45_n_4_beta_half.rda <- load(file="both_effect_q_10_alpha_45_n_4_beta_half.rda")
both_effect_q_10_alpha_45_n_4.rda <- load(file="both_effect_q_10_alpha_45_n_4.rda")

# Add id column
#SI.ts_behaviour_effect_q_10_alpha_45_n_1[,model:="MQ1"]
#SI.ts_behaviour_effect_q_10_alpha_45_n_4[,model:="MQ4"]
#SI.ts_behaviour_effect_q_10_alpha_45_n_4[,model:="MQ4bf"]
#SI.ts_behaviour_effect_q_10_alpha_45_n_4_beta_half[,model:="MQ4bh"]
#SI.ts_behaviour_effect_q_4_alpha_45_n_1[,model:="Mq1"]
#SI.ts_behaviour_effect_q_4_alpha_45_n_4[,model:="Mq4"]
#SI.ts_both_effect_q_10_alpha_45_n_1[,model:="HMQ1"]
SI.ts_both_effect_q_10_alpha_45_n_4[,model:="HMQ4bf"]
SI.ts_both_effect_q_10_alpha_45_n_4_beta_half[,model:="HMQ4bh"]

SI.ts_behaviour_effect_q_10_alpha_45_n_4[,model:="MQ4bf"]
SI.ts_behaviour_effect_q_10_alpha_45_n_4_beta_half[,model:="MQ4bh"]

#SI.ts_both_effect_q_4_alpha_45_n_1[,model:="HMq1"]
#SI.ts_both_effect_q_4_alpha_45_n_4[,model:="HMq4"]
#SI.ts_hetero_effect_q_0_alpha_45_n_1[,model:="H1"]

SI.ts_hetero_effect_q_0_alpha_45_n_4[,model:="H4bf"]
SI.ts_hetero_effect_q_10_alpha_45_n_4_beta_half[,model:="H4bh"]

#SI.ts_no_effect_q_0_alpha_0_n_1[,model:="B1"]

SI.ts_no_effect_q_0_alpha_0_n_4[,model:="B4bf"]
SI.ts_no_effect_q_10_alpha_45_n_4_beta_half[,model:="B4bh"]

rbind(SI.ts_behaviour_effect_q_10_alpha_45_n_4,
      SI.ts_both_effect_q_10_alpha_45_n_4,
      SI.ts_hetero_effect_q_0_alpha_45_n_4,
      SI.ts_no_effect_q_0_alpha_0_n_4,
      SI.ts_behaviour_effect_q_10_alpha_45_n_4_beta_half,
      SI.ts_both_effect_q_10_alpha_45_n_4_beta_half,
      SI.ts_hetero_effect_q_10_alpha_45_n_4_beta_half,
      SI.ts_no_effect_q_10_alpha_45_n_4_beta_half)->plot_HIV_b

plot_HIV_b[,beta_state:=substr(plot_HIV_b$model, nchar(plot_HIV_b$model), nchar(plot_HIV_b$model))]

(ggplot(plot_HIV_b[variable %in% c('S','P','CDR')])
  + aes(x = time, y = value, group=variable,color=variable)
  + geom_line()
  + facet_wrap(~ model,scales = "free")
)

# New facet label names for supp variable
supp.labs <- c("No effect", "Heterogeneity","Both effects","Behaviour")
names(supp.labs) <- c("B4", "H4","HMQ4","MQ4")

# Create the plot
p + facet_grid(
  dose ~ supp, 
  labeller = labeller(dose = dose.labs, supp = supp.labs)
)


pp <- plot_HIV_b[ (variable %in% c('S','P')) & (model %in% c("B4","H4","MQ4","HMQ4")),]
(ggplot(pp)
  + aes(x = time, y = value, group=variable,color=variable)
  + geom_line(group=beta_state)
  + facet_wrap(~ model,labeller = labeller(model = supp.labs), ncol = 4)+
    xlab("Days") + ylab("Proportion of popn. ")+
    scale_fill_viridis() + theme_bw()
)


pcdr <- plot_HIV[ (variable %in% c('CDR')) & (model %in% c("B4","H4","MQ4","HMQ4")),]
(ggplot(pcdr)
  + aes(x = time, y = value, group=variable,color=variable)
  + geom_line()
  + facet_wrap(~ model,labeller = labeller(model = supp.labs), ncol = 4)+
    xlab("Days") + ylab("Proportion of popn. ")+
    scale_fill_viridis() + theme_bw()
)

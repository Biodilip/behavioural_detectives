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
library(purrr)
library(viridis)



rm(list=ls())                   # Clear all variables and functions


#setwd("./Results")
## Script that loads and plots our results 

b <- readRDS("./Results/file_list.rds")


Results  <- list()
#i = 2
counter <-1
for(i in 1:16){
  Results[[i]]<- readRDS(paste0("Results/",b[i]))
  Results[[i]][["model"]] <- counter
  counter <- counter+1
}



#bind effects to be explored

#explore half beta


vary_beta <- rbind(Results[[1]],Results[[2]],Results[[3]],Results[[4]],
                   Results[[5]],Results[[6]],Results[[7]],Results[[8]])

vary_beta.long <- melt(vary_beta, id.vars = 'time')

vary_alpha <- rbind(Results[[1]],Results[[2]],Results[[3]],Results[[4]],
                   Results[[9]],Results[[10]],Results[[11]],Results[[12]])

vary_alpha.long <- melt(vary_alpha, id.vars = 'time')

vary_q <- rbind(Results[[1]],Results[[2]],Results[[3]],Results[[4]],
                   Results[[13]],Results[[14]],Results[[15]],Results[[16]])

vary_q.long <- melt(vary_q, id.vars = 'time')




(ggplot(plot_HIV[variable %in% c('S','P','CDR')])
  + aes(x = time, y = value, group=variable,color=variable)
  + geom_line()
  + facet_wrap(~ model,scales = "free")
)

# New facet label names for supp variable
change.labels <- c("No effect", "Heterogeneity","Both effects","Behaviour")
names(change.labels) <- c("B4", "H4","HMQ4","MQ4")

# Create the plot
p + facet_grid(
  dose ~ supp, 
  labeller = labeller(dose = dose.labs, supp = supp.labs)
)


pp <- plot_HIV[ (variable %in% c('S','P')) & (model %in% c("B4","H4","MQ4","HMQ4")),]
(ggplot(pp)
  + aes(x = time, y = value, group=variable,color=variable)
  + geom_line()
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

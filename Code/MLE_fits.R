

source("./Code/nllikelihood.R")

## First look up how optim() works. The more you read through the help
## file the easier this will be!!! In particular make sure you understand that
## the first argument of optim must be the initial values of the parameters to
## be fitted (i.e. Beta & alpha) and that any other parameters to be fixed are
## given as additional arguments (in the help file under "...")
## ?optim

## Since we need to be able to easily separate fitted and fixed parameters,
## let's make a function that takes fitted and fixed parameters together and
## puts them back in a parameter list (similar to the output of
## disease_params()). We also want it to be able to take logged parameter values
## and unlog them automatically back into the parameter list, so that we can fit
## on a log scale, but run our model with the unlogged values.

subsParms <- function(fit.params, fixed.params=disease_params())
  within(fixed.params, {
    loggedParms <- names(fit.params)[grepl('log_', names(fit.params))]
    unloggedParms <- names(fit.params)[!grepl('log_', names(fit.params))]        
    for(nm in unloggedParms) assign(nm, as.numeric(fit.params[nm]))
    for(nm in loggedParms) assign(gsub('log_','',nm), exp(as.numeric(fit.params[nm])))
    rm(nm, loggedParms, unloggedParms)
  })


guess.params <- c(log_Beta = log(5), log_alpha = log(8))
subsParms(guess.params, disease_params())


## Make likelihood a function of fixed and fitted parameters.
objFXN <- function(fit.params ## paramters to fit
                   , fixed.params =disease_params() ## fixed paramters
                   , obsDat=myDat) {
  parms <- subsParms(fit.params, fixed.params)
  nllikelihood(parms, obsDat = obsDat) ## then call likelihood
}


#result_list <- lapply(list_of_myDat, function(parms) nllikelihood(parms,  obsDat))

# objFXN(guess.params, disease_params())

## Select initial values for fitted parameters from which optimization routine
## will start. If you select bad initial values the algorithm can get stuck on a
## bad set of parameters. You can always try the true values as a starting point
## for this problem, although that's rarely possible in real problems.

init.pars <- c(log_alpha = log(30), log_Beta = log(.1))
## We will start with SANN optimization since it is stochastic and therefore
## less likely to get stuck in a local minima. But then finish with Nelder-Mead
## optimization which is much faster.

###  NOTE: for trace >0 you see more progress report, bigger numbers show more
###  update
trace <- 3

## SANN: This is stochastic, be CAREFUL -- sometimes it gets stuck at local minima
## for unreasonble parameters. If you see this happen, run it again!




optimization <- function(){
  optim.vals <- optim(par = init.pars
                      , objFXN
                      , fixed.params = disease_params()
                      , obsDat = myDat
                      , control = list(trace = trace, maxit = 150)
                      , method = "SANN")
  ## We feed the last parameters of SANN in as the first values of Nelder-Mead
  optim.vals <- optim(par = optim.vals$par
                      , objFXN
                      , fixed.params = disease_params()
                      , obsDat = myDat
                      , control = list(trace = trace, maxit = 800, reltol = 10^-7)
                      , method = "Nelder-Mead"
                      , hessian = T)
  
  # extract MLE fits
  MLEfits <- exp(unname(optim.vals$par))
  names(MLEfits) <- c("MLE_alpha", "MLE_beta")
  
  # True paramaters
  trueParms2 <- as.data.frame(trueParms[c('alpha','Beta')])
  
  # output
  out <- cbind(t(MLEfits), trueParms2)
  # out_list <- list(optim.vals$par, out)
  # names(out_list) <- c('optim.vals', 'MLEfit') 
  
  return(out)
}


list_of_MLE_fits <- list()
count= 1
for (df in seq(1,length(list_of_myDat))){
  myDat = list_of_myDat[[df]]
  objFXN(guess.params, disease_params())
  out <- optimization()
  list_of_MLE_fits[[count]] <- out
  count =count +1
}


#MLE_fits<- optimization()

# MLE value / true value 


log_alpha.fit <- MLE_fits[[1]][[1]]
log_Beta.fit <- MLE_fits[[1]][[2]]

## Look at the output of optim. Understand what it means. Did the algorithm
## converge? Look at ?optim to understand it.

## Plot MLE fit time series

fitDat <- simEpidemic(pop.SI0, parms = subsParms(MLE_fits[[1]], trueParms))


# return((trueParms, guess.params, exp(unname(MLEfits)), fisherInfMatrix))



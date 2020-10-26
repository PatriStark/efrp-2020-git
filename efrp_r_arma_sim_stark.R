#### SIMULATE ARMA MODELS ####
library(tidyverse)
library(astsa)
library(tseries)
library(forecast)
library(dplyr)


### Store parameters ###

arma_sim_param <- function(N, M, sigma, p, q){
  #  stores the parameters of the simulation
  
  #  N ... number of generated realisations
  #  M ... length of each time series
  #  sigma ... standard deviation of the _normal_ white noise
  #  p ... order of the AR component
  #  q ... order of the MA component
  
  parameters <- tibble::tibble("number" = N, "length" = M, "sigma" = sigma, "AR" = p, "MA" = q)
  
  # returns a tibble
  return(parameters) 
}

parameters <- arma_sim_param(N = 10, M = 100, sigma = 0.2, p = 0, q = 0)

### Generate samples ###

arma_simulation <- function(parameters){
  ## generate N samples of length M of an ARMA(p,q) process, with a normal WN(sigma^2)
  
  # read parameters in
  #N <- parameters$number
  M <- parameters$length
  sigma <- parameters$sigma
  p <- parameters$AR
  q <- parameters$MA
  d <- 0
  
  # generate coefficients randomly from  _normal_ distribution
  
  theta <- rnorm(q) # theta ... MA coefficients
  phi <- rnorm(p, 0, 0.33) # phi ... AR coefficients
  roots <- polyroot(c(1, -phi))
  while ( !prod((abs(roots)>1))  ){
    # regenerate while the model is not stationary
    phi <- rnorm(p) # phi ... AR coefficients
    roots <- polyroot(c(1, -phi))
  }
  
  # simulate time series
  
  ts <- arima.sim(model = list(order = c(p, d, q), ma = theta, ar = phi), n = M, sd = sigma)
  return(ts)
}

ts <- arma_simulation(parameters)


### MODEL FITTING PARAMETERS ###

arima_model_fitting_parameters <- function(IC = "AIC", max_p = 2, max_q = 2){
  ## stores model fitting parameters
  #  IC ... information criterion (AIC, BIC, HQ)
  #  max_p ... maximum order of AR
  #  max_q ... maximum order of MA
  
  model_parameters <- tibble::tibble("IC" = IC, "max p" = max_p, "max q" = max_q)
  return(model_parameters)
}

model_parameters <- arima_model_fitting_parameters()

### FITTING MODEL ###

fitting_model <- function(model_parameters){
  # Fits the model and selects according to IC
  # Returns whether model specification is accurate
  
  # "model_parameters" has attributes:
    # $IC ... either "AIC" or "BIC" ... information criterion
    # $`max p` ... maximum order of AR
    # $`max q` ... maximum order of MA
  
  # maximum orders
  max_p = model_parameters$`max p`
  max_q = model_parameters$`max q`
  
  # the IC
  IC = model_parameters$IC
  
  
  # initialise order parameters and IC_value
  IC_value = 10000000000 # a really large number
  p_chosen = 0
  q_chosen = 0
  
  #for every possible order: fit model and calculate IC
  for (i in 0:max_p){
    for (j in 0:max_q){
      
      #fit model
      fitted_model <- ts %>% sarima(i,0,j, details = FALSE)
      
      # refresh IC if lower than any other before
      if (IC == "AIC"){# AIC case
        if (IC_value > fitted_model$AIC){
          IC_value = fitted_model$AIC
          p_chosen = i
          q_chosen = j
        }
      }else{ # BIC case
        if (IC_value > fitted_model$BIC){
          IC_value = fitted_model$BIC
          p_chosen = i
          q_chosen = j
        }
      }
    }}

  
  # check whether specification is correct
  if (p_chosen == parameters$AR & q_chosen == parameters$MA){
    # if yes
    return(1)
  }else{
    # if not
    return(0)
  }
  
}

results <- tibble::tibble("N"= "","Length" = "", "Sigma" = "", "AIC" = "", "BIC" = "")

monte_carlo_arma <- function(){
  AIC_Sum = 0
  BIC_Sum = 0
  for (h in 1:parameters$number){
    model_parameters <- arima_model_fitting_parameters("AIC")
    AIC_Sum = fitting_model() + AIC_Sum
    model_parameters <- arima_model_fitting_parameters("BIC")
    BIC_Sum = fitting_model() + BIC_Sum
  }
  results <- rbind(results, c(parameters$number, parameters$length, parameters$sigma, AIC_Sum, BIC_Sum))
  return(results)
}

results <- monte_carlo_arma()


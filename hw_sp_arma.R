#### SIMULATE ARMA MODELS ####
library(tidyverse)
library(astsa)
library(tseries)
library(forecast)
library(dplyr)

setwd('C:/Users/Patricia Stark/Desktop/ARMA')
source('monte_carlo_arma.R')
source('fitting_model.R')
source('arima_model_fitting_parameters.R')
source('arma_simulation.R')
source('arma_sim_param.R')


### FITTING MODEL ###

results <- tibble::tibble("N"= "","Length" = "", "Sigma" = "", "AIC" = "", "BIC" = "") 


results <-  monte_carlo_arma()




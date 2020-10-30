fitting_model <- function(ts, model_parameters, parameters){
  # Fits the model and selects according to IC
  # Returns whether model specification is accurate
  
  # "model_parameters" has attributes:
    # $IC ... either "AIC" or "BIC" ... information criterion
    # $`max p` ... maximum order of AR
    # $`max q` ... maximum order of MA
  
  # 'parameters' has attributes:
    # $number ... N ... number of generated realisations
    # $length ... M ... length of each realisation
    # $AR ... p ... order of AR
    # $MA ... q ... order of MA
    # $sigma ... sigma ... standard deviation of _normal_ white noise
  
  # 'ts' is an ARMA(p,q) time series
  
  
  
  # maximum orders
  max_p = model_parameters$`max p`[1]
  
  max_q = model_parameters$`max q`[1]
  
  # the IC
  IC = model_parameters$IC[1]
  
  
  # fit model 
  fitted_model <- ts %>% auto.arima(d = 0, D = 0, max.p = max_p, max.q = max_q, max.P = 0, max.Q = 0, ic = IC )
      
  # get estimated orders
  p_chosen = fitted_model[['arma']][1]
  q_chosen = fitted_model[['arma']][2]
  
  # check whether specification is correct
  if (p_chosen == 1 | q_chosen == 1){
    # if yes
    return(1)
  }else{
    # if not
    return(0)
  }
  
}


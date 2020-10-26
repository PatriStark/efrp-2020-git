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
  max_p = model_parameters$`max p`
  max_q = model_parameters$`max q`
  
  # the IC
  IC = model_parameters$IC
  
  
  # fit model 
  fitted_model <- ts %>% auto.arima(d = NA, max.p = max_p, max.q = max_q, ic = IC )
      
  # get estimated orders
  p_chosen = fitted_model[1]
  q_chosen = fitted_model[2]
  
  # check whether specification is correct
  if (p_chosen == parameters$AR & q_chosen == parameters$MA){
    # if yes
    return(1)
  }else{
    # if not
    return(0)
  }
  
}
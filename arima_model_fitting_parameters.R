arima_model_fitting_parameters <- function(IC = "AIC", max_p = 2, max_q = 2){
  #  stores model fitting parameters
  
  #  IC ... information criterion (AIC, BIC, HQ)
  #  max_p ... maximum order of AR
  #  max_q ... maximum order of MA
  
  
  model_parameters <- tibble::tibble("IC" = IC, "max p" = max_p, "max q" = max_q)
  
  # returns a tibble
  return(model_parameters)
}

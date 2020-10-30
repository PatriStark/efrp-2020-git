monte_carlo_arma <- function(){
  # Simulate model fitting for N times
  
  # Accuracy
  AIC_Sum = 0
  BIC_Sum = 0
  
  
  # Store parameters
  parameters <- arma_sim_param(N = 1000, M = 100, sigma = 0.25, p = 1, q = 1)
  
  # MC simulation
  for (h in 1:parameters$number[1]){
    
    # Generate sample
    ts <- arma_simulation(parameters)
    
    # AIC
    model_parameters <- arima_model_fitting_parameters("aic")
    AIC_Sum = fitting_model(ts, model_parameters, parameters)[1] + AIC_Sum
    
    # BIC
    model_parameters <- arima_model_fitting_parameters("bic")
    BIC_Sum = fitting_model(ts, model_parameters, parameters)[1] + BIC_Sum
  }
  results <- rbind(results, c(parameters$number[1], parameters$length[1], parameters$sigma[1], AIC_Sum, BIC_Sum))
  return(results)
}


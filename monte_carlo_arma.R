monte_carlo_arma <- function(){
  # Simulate model fitting for N times
  
  # Accuracy
  AIC_Sum = 0
  BIC_Sum = 0
  
  
  # Store parameters
  parameters <- arma_sim_param(N = 10, M = 100, sigma = 0.01, p = 0, q = 0)
  
  # MC simulation
  for (h in 1:parameters$number){
    
    # Generate sample
    ts <- arma_simulation(parameters)
    
    # AIC
    model_parameters <- arima_model_fitting_parameters("AIC")
    AIC_Sum = fitting_model(ts, model_parameters, parameters) + AIC_Sum
    
    # BIC
    model_parameters <- arima_model_fitting_parameters("BIC")
    BIC_Sum = fitting_model(ts, model_parameters, parameters) + BIC_Sum
  }
  results <- rbind(results, c(parameters$number, parameters$length, parameters$sigma, AIC_Sum, BIC_Sum))
  return(results)
}
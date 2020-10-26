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
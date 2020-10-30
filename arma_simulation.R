arma_simulation <- function(parameters, phi = c(0.5), theta = c(0.85)){
  ## generate N samples of length M of an ARMA(p,q) process, with a normal WN(sigma^2)
  
  # "parameters" has attributes:
    # $number ... N ... number of generated realisations
    # $length ... M ... length of each realisation
    # $AR ... p ... order of AR
    # $MA ... q ... order of MA
    # $sigma ... sigma ... standard deviation of _normal_ white noise
  
  
  # read parameters in
  M <- parameters$length[1]
  sigma <- parameters$sigma[1]
  p <- parameters$AR[1]
  q <- parameters$MA[1]
  d <- 0
  
  # generate coefficients randomly from  _uniform_ distribution
  
  #theta <- runif(q, -2.5,2.5) # theta ... MA coefficients
  #phi <- runif(p, -0.6, 0.6) # phi ... AR coefficients
  
  # decide whether AR is stationary
  #roots <- c(1, -phi) %>% polyroot()
  
  # regenerate while the model is not stationary
  #while ( ! (roots %>% abs() %>% prod()) > 1){
    
    #phi <- runif(p,-0.5,-.5) # phi ... AR coefficients
    #roots <- c(1, -phi) %>% polyroot()
  #}
  
  # simulate time series
  
  ts <- arima.sim(model = list(order = c(p, d, q), ma = theta, ar = phi), n = M, sd = sigma)
  
  # returns a time series
  return(ts)
}


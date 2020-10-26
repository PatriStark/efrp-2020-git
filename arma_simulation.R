arma_simulation <- function(parameters){
  ## generate N samples of length M of an ARMA(p,q) process, with a normal WN(sigma^2)
  
  # "parameters" has attributes:
    # $number ... N ... number of generated realisations
    # $length ... M ... length of each realisation
    # $AR ... p ... order of AR
    # $MA ... q ... order of MA
    # $sigma ... sigma ... standard deviation of _normal_ white noise
  
  
  # read parameters in
  M <- parameters$length
  sigma <- parameters$sigma
  p <- parameters$AR
  q <- parameters$MA
  d <- 0
  
  # generate coefficients randomly from  _normal_ distribution
  
  theta <- rnorm(q) # theta ... MA coefficients
  phi <- rnorm(p, 0, 0.33) # phi ... AR coefficients
  
  # decide whether AR is stationary
  roots <- c(1, -phi) %>% polyroot()
  
  # regenerate while the model is not stationary
  while ( ! (roots %>% abs() %>% prod()) > 1){
    
    phi <- rnorm(p) # phi ... AR coefficients
    roots <- c(1, -phi) %>% polyroot()
  }
  
  # simulate time series
  
  ts <- arima.sim(model = list(order = c(p, d, q), ma = theta, ar = phi), n = M, sd = sigma)
  
  # returns a time series
  return(ts)
}

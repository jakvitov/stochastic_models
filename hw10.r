SimulateARMANoWhiteNoise <- function(n, alpha = 0, phi = c(), theta = c(), sigma = 1, init.obs = c()) {
  
  p <- length(phi) # rad autoregresni posloupnosti
  q <- length(theta) # rad posloupnosti klouzavych souctu
  
  init.n <- length(init.obs) # pocet pocatecnich pozorovani
  
  e <- rnorm(n = n, mean = 0, sd = sigma) # simulace normalniho bileho sumu
  
  x <- rep(NA, n) # inicializace vektoru x
  if (init.n > 0) {
    x[1:init.n] <- init.obs
  }
  
  for (t in (init.n + 1):n) {
    x[t] <- alpha + sum(phi * x[(t - 1):(t - p)]) # ARMA rekurze
  }
  
  if (p == 0 & q == 0) {
    message("White Noise")
  } else if (p > 0 & q == 0) {
    message("AR(", p, ") Process")
  } else if (p == 0 & q > 0) {
    message("MA(", q, ") Process")
  } else if (p > 0 & q > 0) {
    message("ARMA(", p, ",", q, ") Process")
  }
  
  res <- data.frame(t = 1:n, e = e, x = x)
  
  return(res)
  
}

SimulateARMA <- function(n, alpha = 0, phi = c(), theta = c(), sigma = 1, init.obs = c()) {
  
  p <- length(phi) # rad autoregresni posloupnosti
  q <- length(theta) # rad posloupnosti klouzavych souctu
  
  init.n <- length(init.obs) # pocet pocatecnich pozorovani
  
  e <- rnorm(n = n, mean = 0, sd = sigma) # simulace normalniho bileho sumu
  
  x <- rep(NA, n) # inicializace vektoru x
  if (init.n > 0) {
    x[1:init.n] <- init.obs
  }
  
  for (t in (init.n + 1):n) {
    x[t] <- alpha + sum(phi * x[(t - 1):(t - p)]) + sum(theta * e[(t - 1):(t - q)]) + e[t] # ARMA rekurze
  }
  
  if (p == 0 & q == 0) {
    message("White Noise")
  } else if (p > 0 & q == 0) {
    message("AR(", p, ") Process")
  } else if (p == 0 & q > 0) {
    message("MA(", q, ") Process")
  } else if (p > 0 & q > 0) {
    message("ARMA(", p, ",", q, ") Process")
  }
  
  res <- data.frame(t = 1:n, e = e, x = x)
  
  return(res)
  
}

TansftormIndex = function(timeline_size, i){
  return(timeline_size + i)
}

#Return dataframe containing time t and associated values x
#Parameters - alpha, phi, theta are parameters of ARMA process
#Sigma is size of pulse
#Pulse occurs in time 0 and timeline starts at -max(p,q, 10)
#Timeline ends at max(p,q,10)
ImpulseResponse <- function(alpha = 0, phi = c(), theta = c(), sigma = 1) {
  p = length(phi)
  q = length(theta)
  timeline_size = max(p, q, 10)
  #Actual values to be returned
  timeline = c()
  x_values = c()
  #Fill the values for time before 0
  for (i in -timeline_size:-1){
    timeline = c(timeline, i)
    x_val = alpha /(1-sum(phi))
    x_values = c(x_values, x_val)
  }
  #Impulse
  timeline = c(timeline, 0)
  x_val =  SimulateARMA(1, alpha, phi, theta, sigma, c(sigma))$e[1]
  x_values = c(x_values, x_val)
  
  impulse <- data.frame(t = timeline, x = x_values)
  
  timeline = c(timeline, 1:(5*timeline_size))
  x_val = SimulateARMANoWhiteNoise(6*timeline_size, alpha, phi, theta, sigma, x_values)$x[-(1:timeline_size + 1)]
  x_values = c(x_values, x_val)
  
  
  impulse <- data.frame(t = timeline, x = x_values)
  
  return(impulse)
  
}

plot(ImpulseResponse(alpha = -0.5, phi = -0.8), type = "b")
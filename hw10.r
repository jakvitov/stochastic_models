
ImpulseResponse <- function(alpha = 0, phi = c(), theta = c(), sigma = 1) {
  
  p <- length(phi) # rad autoregresni posloupnosti
  q <- length(theta) # rad posloupnosti klouzavych souctu
  
  timeline_size = max(p,q,10)
  
  timeline_size # pocet pocatecnich pozorovani
  
  #Inicializace vektoru e
  e <- rep(0, (timeline_size*5)+1)
  e[1] = sigma
  
  #Inicializace prvních - hodnot
  x <- rep(NA, timeline_size) 
  #Hodnoty -timeline_size:-1
  x[1:timeline_size] <- alpha/(1-sum(phi))
  
  x[(timeline_size + 1)] = alpha + sum(phi * x[(1):(1 - p)]) + sum(theta * e[(1 - 1):(1 - q)]) + e[1] # ARMA rekurze
  
  start_t = timeline_size + 2
  end_t = (6*timeline_size)+1
  for (t in start_t:end_t){
    x[t] = alpha + sum(phi * x[(t - 1):(t - p)])
  }
  
  
  t = -timeline_size:(5*timeline_size)
  impulse <- data.frame(t = t, x = x)
  
  return(impulse)
  
}

plot(ImpulseResponse(alpha = -0.5, phi = -0.8), type = "b")
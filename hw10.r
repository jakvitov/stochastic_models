
ImpulseResponse <- function(alpha = 0, phi = c(), theta = c(), sigma = 1) {
  
  p <- length(phi) # rad autoregresni posloupnosti
  q <- length(theta) # rad posloupnosti klouzavych souctu
  
  timeline_size = max(p,q,10)
  zero_index = timeline_size + 1
  
  #Time and x values and e_values
  time_values = (-timeline_size:(5*timeline_size))
  e_values = rep(0, (timeline_size*6) + 1)
  x_values = rep(alpha/(1-sum(phi)), (timeline_size*6) + 1)
  #Only e_values are sigma
  e_values[zero_index] = sigma
  
  end_index = length(time_values)
  #Fill in the missing x
  for (t in zero_index:end_index){
    x_values[t] <- alpha + sum(phi * x_values[(t - 1):(t - p)]) + sum(theta * e_values[(t - 1):(t - q)]) + e_values[t]
  }
  
  impulse <- data.frame(t = time_values, x = x_values)
  
  
  return(impulse)
  
}

plot(ImpulseResponse(alpha = -0.5, phi = -0.8), type = "b")
plot(ImpulseResponse(theta = seq(from = 2, to = 0.1, by = -0.1)), type = "b")

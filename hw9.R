  StationaryBirthDeath <- function(up, down, return.cut = 1e2) {
    
    alpha0 <- 1 / sum(c(1, cumprod(up / down)))
    
    alpha <- c(1, cumprod(up / down)) * alpha0
    
    return(alpha[1:(1 + return.cut)])
    
  }
  
  StationaryMMc <- function(lambda, mu, c, sum.cut = 1e5, return.cut = 1e2) {
    
    up <- rep(lambda, sum.cut)
    down <- mu * c(1:c, rep(c, sum.cut - c))
    
    alpha <- StationaryBirthDeath(up = up, down = down, return.cut = return.cut)
    
    return(alpha)
    
  }
  
  
  AltCostMMc <- function(lambda, mu, cost1, cost2, max.c) {
    
    cost <- data.frame(c = integer(), cost.queue = numeric(), cost.cashier = numeric(), cost.total = numeric(), optimal = logical())
    
    min.c <- floor(lambda / mu + 1)
    
    for (c in min.c:max.c) {
      stationary = StationaryMMc(lambda = lambda, mu = mu, c = c, return.cut = 1e5)
      queue_stationary = stationary[-(1: (c+1))]
      q = sum(queue_stationary * log(seq_along(queue_stationary)))
      cost.queue = cost2*q
      cost.cashier <- cost1 * c
      cost.total <- cost.queue + cost.cashier
      
      cost <- rbind(cost, data.frame(c = c, cost.queue = cost.queue, cost.cashier = cost.cashier, cost.total = cost.total, optimal = FALSE))
      
    }
    
    cost$optimal[which.min(cost$cost.total)] <- TRUE
    
    return(cost)
    
  }
  
  
  lambda <- 150 # intenzita prichodu
  mu <- 20 # intenzita obsluhy jedne pokladny
  
  cost1 <- 70000 / 30 # denni naklady na provoz jedne poklady - 70 000 Kč za měsíc
  cost2 <- 10000 * 24 # denni naklady za vyskyt dlouhe fronty - 10 000 Kč za hodinu
  long.queue <- 10 # delka dlouhe fronty (10 a vice)
  max.c <- 30
  
  cost <- AltCostMMc(lambda, mu, cost1, cost2, max.c)
  cost
  
  plot(1:101, StationaryMMc(lambda, mu, 10))

SimulateMM1 <- function(lambda, mu, max.time = 100) {
  
  cur.time <- 0 # aktualni cas - zacnu v 0
  cur.event <- "open" # aktualni typ udalosti - zacnu otevrenim obchodu
  cur.state <- 0 # aktualni stav - zacnu v prazdnem obchode
  cur.customer <- 0 # aktualne prichozi - na zacatku nikdo
  
  next.arrive <- Inf # dalsi prichod, na zacatku nevim a dam nekonecno
  next.depart <- Inf # dalsi odchod, na zacatku nevim a dam nekonecno
  
  in.service <- c() # zakaznik v obsluze
  in.queue <- c() # zakaznici ve fronte
  
  events <- data.frame(time = numeric(), event = character()) # vysledky - udalosti
  states <- data.frame(state = integer(), time.start = numeric(), time.end = numeric()) # vysledky - navstivene stavy
  customers <- data.frame(id = integer(), time.arrive = numeric(), time.cash = numeric(), time.depart = numeric(), queue.length = integer()) # vysledky - info o zakaznicich
  
  repeat { # opakuj donekonecna
    
    events <- rbind(events, data.frame(time = cur.time, event = cur.event)) # ulozim vysledky o udalosti
    
    if (cur.event == "open") { # obchod otevrel
      
      next.arrive <- rexp(n = 1, rate = lambda) # cas prvniho prichodu
      
    } else if (cur.event == "arrive") { # nekdo prisel
      
      cur.customer <- cur.customer + 1 # cislo prichoziho
      customers <- rbind(customers, data.frame(id = cur.customer, time.arrive = cur.time, time.cash = NA, time.depart = NA, queue.length = cur.state)) # ulozim prichod zakaznika
      next.arrive <- cur.time + rexp(n = 1, rate = lambda) # cas dalsiho prichodu
      if (cur.state == 0) { # prichozi jde rovnou do obsluhy
        in.service <- cur.customer
        next.depart <- cur.time + rexp(n = 1, rate = mu) # cas dalsiho odchodu
        customers$time.cash[in.service] <- cur.time # ulozim cas, kdy zakaznik prijde na radu
        customers$time.depart[in.service] <- next.depart # ulozim cas odchodu
      } else { # prichozi jde do fronty
        in.queue <- c(in.queue, cur.customer) # pridam do fronty
      }
      cur.state <- cur.state + 1 # zvysime pocet lidi v obchode o jedna
      
    } else if (cur.event == "depart") { # nekdo odesel
      
      if (cur.state == 1) { # obslouzeny clovek je posledni v obchode
        in.service <- c()
        next.depart <- Inf # obchod je prazdny a dalsi clovek odejde v nedohlednu
      } else {
        in.service <- in.queue[1] # prvni clovek ve fronte prijde na radu - FIFO
        in.queue <- in.queue[-1] # odeberu cloveka z fronty - FIFO
        # in.service <- in.queue[length(in.queue)] # LIFO
        # in.queue <- in.queue[-length(in.queue)] # LIFO
        # in.service <- sample(in.queue, size = 1) # SIRO
        # in.queue <- in.queue[in.queue != in.service] # SIRO
        next.depart <- cur.time + rexp(n = 1, rate = mu) # cas dalsiho odchodu
        customers$time.cash[in.service] <- cur.time # ulozim cas, kdy zakaznik prijde na radu
        customers$time.depart[in.service] <- next.depart # ulozim cas odchodu
      }
      cur.state <- cur.state - 1 # snizime pocet lidi v obchode o jedna
      
    } else if (cur.event == "close") { # obchod zavrel
      
      customers$time.cash[is.na(customers$time.cash) | customers$time.cash > max.time] <- max.time
      customers$time.depart[is.na(customers$time.depart) | customers$time.depart > max.time] <- max.time
      if (cur.state > 0) {
        states <- rbind(states, data.frame(state = seq(from = cur.state - 1, to = 0, by = -1), time.start = max.time, time.end = max.time))
      }
      break() # ukoncime cyklus
      
    }
    
    possible.time <- c(next.arrive, next.depart, max.time) # jake udalosti mam pred sebou
    next.time <- min(possible.time) # v jaky cas nastane nasledujici udalost
    possible.event <- c("arrive", "depart", "close") # mozne nasledujici udalosti
    next.event <- possible.event[which.min(possible.time)] # typ nasledujici udalosti
    
    states <- rbind(states, data.frame(state = cur.state, time.start = cur.time, time.end = next.time)) # ulozim vysledky o navstivenem stavu
    
    cur.time <- next.time # z casu nasledujici udalosti udelam soucasny
    cur.event <- next.event # z typu nasledujici udalosti udelam soucasny
    
  }
  
  states <- cbind(states, duration = states$time.end - states$time.start) # pridam sloupec s dobou trvani stavu
  customers <- cbind(customers, duration.queue = customers$time.cash - customers$time.arrive, duration.service = customers$time.depart - customers$time.cash, duration.total = customers$time.depart - customers$time.arrive) # pridam sloupce s dobami trvani
  
  return(list(events = events, states = states, customers = customers))
  
}


lambda <- 15 # intenzita prichodu - v prumeru prijde 15 lidi za hodinu / prumerna doba mezi prichody je 4 minuty
mu <- 20 # intenzita obsluhy - v prumeru je pokladna schopna obslouzit 20 lidi za hodinu / prumerna doba obsluhy je 3 minuty
max.time <- 10 # cas, kdy obchod zavira / cas, kdy ukoncim simulaci
results <- SimulateMM1(lambda = lambda, mu = mu, max.time = max.time)

NoQueueTime <- function(results) {
  
  states <- results$states
  times = c()
  start = 0
  for (i in 1:length(states$state - 1)){
    if (states$state[i] == 1 && states$state[i + 1] == 2){
      times = c(times, states$time.start[i + 1] - start)
    }
    else if (states$state[i] == 2 && states$state[i + 1] == 1){
      start = states$time.end[i]
    }
    else if (i+1 == length(states$state)){
      times = c(times, states$time.end[i + 1] - start)
    }
  }
  no_queue_mean <- mean(times)
  return(no_queue_mean)
  
}



#labda - Počet volajících za hodinu v průměru
#mu - Počet průměrně obsloužených za hodinu
#o - počet telefonistů
#alpha - intenzita zavěšení
#w - kapacita fronty
CallCenterQueue <- function(lambda, mu, alpha, o, w) {
  
  s <- o + 1# pocet stavu
  
  Q <- matrix(0, nrow = s + w, ncol = s + w) # inicializace matice intenzit
  
  Q[1, 1] <- -lambda
  Q[1, 2] <- lambda
  
  for (i in 2:(s - 1)) {
    Q[i, i - 1] <- (i - 1) * mu
    Q[i, i + 1] <- lambda
    Q[i, i] <- -(Q[i, i - 1] + Q[i, i + 1])
  }
  
  Q[s, s - 1] <- (o * mu)
  Q[s, s] <- (-o * mu)
  
  #Queue calculations
  for (i in s:(s+w - 1)) {
    Q[i, i - 1] <- ((o * mu) + (i-s)*alpha)
    Q[i, i + 1] <- lambda
    Q[i, i] <- -(Q[i, i - 1] + Q[i, i + 1])
  }
  
  Q[s+w, s+w - 1] <- (o * mu)+(w*alpha)
  Q[s+w, s+w] <- -Q[s+w, s+w - 1]
  
  return(Q)
}

#Q <- CallCenterQueue(lambda = 4, mu = 2, o = 3, alpha=5, w=2)
#Q
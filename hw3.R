
###
# CVICENI 3 - ABSORPCNI MARKOVSKE RETEZCE
###



# FUNKCE PRO VYPOCET PRECHODOVE MATICE STRATEGIE RULETY "RED ONE-BY-ONE"

# goal - cilova castka
# win - pravdepodobonst vyhry

RouletteRedOneByOne <- function(goal, win = 18 / 37) {
  
  P <- matrix(0, nrow = goal + 1, ncol = goal + 1) # inicializace, same nuly
  
  P[1, 1] <- 1 # prvni radek
  
  for (kc in 1:(goal - 1)) { # druhy az predposledni radek
    P[kc + 1, kc] <- 1 - win # prohra
    P[kc + 1, kc + 2] <- win # vyhra
  }
  
  P[goal + 1, goal + 1] <- 1 # posledni radek
  
  state.names <- paste0(0:goal, "Kc") # spoji cisla od 0, 1, ... s retezcem " Kc"
  rownames(P) <- state.names # pojmenuje radky
  colnames(P) <- state.names # pojmenuje sloupce
  
  return(P)
  
}



# FUNKCE PRO VYPOCET PRECHODOVE MATICE STRATEGIE RULETY "RED ALL-IN"

# goal - cilova castka
# win - pravdepodobonst vyhry

RouletteRedAllIn <- function(goal, win = 18 / 37) {
  
  P <- matrix(0, nrow = goal + 1, ncol = goal + 1) # inicializace, same nuly
  
  P[1, 1] <- 1 # prvni radek
  
  for (kc in 1:(floor(goal / 2))) { # vsadi vse
    P[kc + 1, 1] <- 1 - win # prohra
    P[kc + 1, kc * 2 + 1] <- win # vyhra
  }
  
  for (kc in (floor(goal / 2) + 1):(goal - 1)) { # vsadi na vyhru cile
    P[kc + 1, kc - (goal - kc) + 1] <- 1 - win # prohra
    P[kc + 1, goal + 1] <- win # vyhra
  }
  
  P[goal + 1, goal + 1] <- 1 # posledni radek
  
  state.names <- paste0(0:goal, "Kc") # spoji cisla od 0, 1, ... s retezcem "Kc"
  rownames(P) <- state.names # pojmenuje radky
  colnames(P) <- state.names # pojmenuje sloupce
  
  return(P)
  
}



# VYPOCET PRECHODOVE MATICE RULETY

P1 <- RouletteRedOneByOne(goal = 9)
P1

P2 <- RouletteRedAllIn(goal = 9)
P2



# FUNKCE PRO VYPOCET FUNDAMENTALNI MATICE ABSORPCNIHO RETEZCE

# P - prechodova matice

AbsFundamental <- function(P) {
  
  s <- nrow(P) # pocet stavu
  
  is.abs <- diag(P) == 1 # jsou jednotlive stavy absorpcni?
  which.abs <- which(is.abs) # ktere stavy jsou absorpcni?
  num.abs <- sum(is.abs) # pocet absorpcnich stavu
  
  is.tr <- !is.abs # jsou jednotlive stavy tranzientni?
  which.tr <- which(is.tr) # ktere stavy jsou tranzientni?
  num.tr <- length(which.tr) # pocet tranzientich stavu
  
  Q <- P[is.tr, is.tr] # pravdepodobnosti prechodu z tranzientnich stavu do tranzientnich
  #Q <- P[which.tr, which.tr] # alternativne
  
  N <- solve(diag(num.tr) - Q) # fundamentalni matice, solve() vrati inverzi matice
  
  return(N)
  
}



# VYPOCET FUNDAMENTALNI MATICE ABSORPCNIHO RETEZCE

N1 <- AbsFundamental(P1)
N1
rowSums(N1)

N2 <- AbsFundamental(P2)
N2
rowSums(N2)



# FUNKCE PRO VYPOCET PRAVDEPODOBNOSTI ABSORPCE

# P - prechodova matice

AbsProb <- function(P) {
  
  N <- AbsFundamental(P) # fundamentalni matice
  
  R <- P[diag(P) < 1, diag(P) == 1]
  
  B <- N %*% R
  
  return(B)
  
}



# VYPOCET PRAVDEPODOBNOSTI ABSORPCE

B1 <- AbsProb(P1)
B1

B2 <- AbsProb(P2)
B2



# FUNKCE PRO SIMULACI ABSORPCNIHO RETEZCE

# P - prechodova matice
# p.init - vektor pocatecniho rozdeleni

SimulateAbs <- function(P, p.init) {
  
  s <- nrow(P) # pocet stavu
  
  is.abs <- diag(P) == 1 # jsou jednotlive stavy absorpcni?
  which.abs <- which(is.abs) # ktere stavy jsou absorpcni?
  num.abs <- sum(is.abs) # pocet absorpcnich stavu
  
  is.tr <- !is.abs # jsou jednotlive stavy tranzientni?
  which.tr <- which(is.tr) # ktere stavy jsou tranzientni?
  num.tr <- length(which.tr) # pocet tranzientich stavu
  
  x <- sample(1:s, size = 1, prob = p.init) # simulace prvniho pozorovani (nepodminene, pouzijeme vektor p.init)
  
  while (x[length(x)] %in% which.tr) { # opakuj, dokud aktualni stav nebude absorpcni
    #while (is.tr[x[length(x)]]) { # alternativne
    
    x.cur <- x[length(x)] # aktualni stav
    x.new <- sample(1:s, size = 1, prob = P[x.cur, ]) # simulace dalsich pozorovani (podminene, pouzijeme matici P)
    
    x <- c(x, x.new) # pridej x.new na konec x
    
  }
  
  if (!is.null(rownames(P))) {
    x <- factor(rownames(P)[x], levels = rownames(P))
  }
  
  return(x)
  
}



# SIMULACE ABSORPCNIHO RETEZEC

pi <- c(0, 0, 0, 1, 0, 0, 0, 0, 0, 0)

SimulateAbs(P1, pi)

SimulateAbs(P2, pi)



# FUNKCE PRO ODHAD CHARAKTERISTIK ABSORPCNIHO RETEZEC POMOCI SIMULACI

# P - prechodova matice
# p.init - vektor pocatecniho rozdeleni
# n - pocet opakovani simulaci

SimulateAbsProperties <- function(P, p.init, n = 1000) {
  
  num.steps <- rep(NA, n) # inicializace, pocet kroku v jednotlivych simulacich
  end.state <- rep(NA, n) # inicializace, v jakem absorpcnim stavu se skonci v jednotlivych simulacich
  
  for (i in 1:n) {
    
    x.names <- SimulateAbs(P, p.init)
    x <- as.integer(x.names)
    
    num.steps[i] <- length(x) - 1 # pocet kroku
    end.state[i] <- x[length(x)] # posledni prvek vektoru x
    
  }
  
  end.state <- factor(rownames(P)[end.state], levels = rownames(P))
  
  hist(num.steps, breaks = 1:max(num.steps)) # vykresli histogram
  
  mean.num.steps <- mean(num.steps) # prumerny pocet kroku
  prob.end.state <- table(end.state) / n # pravdepodobnosti jednotlivych konecnych stavu
  
  res <- list(mean.num.steps = mean.num.steps, prob.end.state = prob.end.state) # slepi dve promenne do seznamu
  
  return(res)
  
}



# ODHAD CHARAKTERISTIK ABSORPCNIHO RETEZEC POMOCI SIMULACI

#SimulateAbsProperties(P1, pi, n = 10000)

#SimulateAbsProperties(P2, pi, n = 10000)

RouletteNumbersOneByOne <- function(goal) {
  res = matrix(0, nrow=goal+1, ncol=goal+1)
  res[1,1] = 1
  res[goal+1, goal+1] = 1
  #Bet - 6/37 -> -1 + 5
  #Bet - 12/37 -> -1 +2
  #Bet - 18/37 -> -1 +1
  for (i in 2:goal) {
    for (j in 1:(goal+1)) {
      if (res[i,j] != 0) {
        next
      }
      if (goal - i >= 5) {
        res[i,j] = 6/37
      }
      else if (goal - i >= 2) {
        res[i,j] = 12/37
      }
      else {
        res[i,j] = 18/37
      }
    }
  }
  return(res)
}

print(RouletteNumbersOneByOne(8))

#print(RouletteRedAllIn(5))


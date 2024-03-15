library("expm")


#Returns average limit matrix, which consists of sum of P^i -> n devided by n+1
AverageLimitMatrix <- function(P, n) {
    res = P%^%0  
  
    for (i in 1:n){
      res = res + P%^%i
    }
    res = res/(n+1)
    return(res)
}

test_p = matrix(c(0.2, 0.8, 0.4, 0.6), nrow=2, ncol=2, byrow=TRUE)
AverageLimitMatrix(test_p, 10)
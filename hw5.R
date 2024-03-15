library("expm")


#Returns average limit matrix, which consists of sum of P^i -> n devided by n+1
AverageLimitMatrix <- function(P, n) {
    res = P%^%0  
  
    for (i in range1:n){
      res = res + P%^%i
    }
    res = res%/%(n+1)
    return(res)
}



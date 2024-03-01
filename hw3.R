
# Return adjacency matrix to win goal int of money when betting 
#Bet - 6/37 -> -1 + 5, when we can go more than 4 ahead
#Bet - 12/37 -> -1 +2, when we can go more than 2 ahead
#Bet - 18/37 -> -1 +1, when we can go 1 ahead
#Ahead means when given number of money is between us and our goal
RouletteNumbersOneByOne <- function(goal) {
  res = matrix(0, nrow=goal+1, ncol=goal+1)
  res[1,1] = 1
  res[goal+1, goal+1] = 1
  
  for (i in 2:goal) {
    for (j in i:(goal+1)) {
      #Skip absorptive state
      if (res[i,j] != 0) {
        next
      }
      #We are more than 4 ahead
      if ((goal+1)-i >=5){
        res[i, i+5] = 6/37
        res[i, i-1] = 1-(6/37)
        #We are more than 1 ahead
      } else if ((goal+1)-i >= 2) {
        res[i, i+2] = 12/37
        res[i, i-1] = 1-(12/37)
        #We are 1 ahead
      } else {
        res[i, i+1] = 18/37
        res[i, i-1] = 1-(18/37)
      }
    }
  }
  return(res)
}

print(RouletteNumbersOneByOne(9))
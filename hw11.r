#Return realized or bipower realized variance for the given x 
RealizedVariance<- function(x, bipower = FALSE) {
  res = 0
  if (bipower == TRUE) {
    for (i in 3:length(x)) {
      res = res + (abs(x[i] - x[i - 1]) * abs(x[i-1] - x[i-2]))
    }
    return((pi/2) * res)
  }
  
  for (i in 2:length(x)){
    res = res + ((x[i] - x[i-1])^2)
  }
  return(res)
  
}
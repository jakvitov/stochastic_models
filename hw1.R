
#Return vector of zero-inflated values of the poisson distribution with 
#the zero probability rho
rzipois <- function(n, lambda, rho) {
  res_bin = sample(c(0, 1), size = n, replace = TRUE, c(rho, 1-rho))
  return(res_bin * rpois(n, lambda))
}

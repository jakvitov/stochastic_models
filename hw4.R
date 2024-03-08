library("expm")

MatrixError <- function(P_est, P_true) {
  n = nrow(P_est)
  bias = sum((1/(n^2))*(P_est-P_true))
  mae = sum((1/(n^2))*abs(P_est-P_true))
  rmse = sqrt(sum((1/(n^2))*(P_est-P_true)))
  result = list(bias = bias, mae = mae, rmse = rmse)
  
  return(result)
}

test_est = matrix(c(0.1, 0.9, 0.4, 0.6), nrow=2, ncol=2, byrow=TRUE)
test_real = matrix(c(0.15, 0.85, 0.3, 0.7), nrow=2, ncol=2, byrow=TRUE)

print(MatrixError(test_est, test_real))
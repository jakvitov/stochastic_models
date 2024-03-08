library("expm")

MatrixError <- function(P_est, P_true) {
    n = nrow(P_est)
    bias = 0
    mae = 0
    rmse = 0
    for (i in 1:n){
      for (j in 1:n) {
        bias = bias + (P_est[i,j] - P_true[i,j])
        mae = mae + abs(P_est[i,j] - P_true[i,j])
        rmse = rmse + (P_est[i,j] - P_true[i,j])^2
      }
    }
    result = list(bias = (1/n^2)*bias, mae = (1/n^2)*mae, rmse = ((1/n^2)*rmse)^(1/2))
  
  return(result)
}

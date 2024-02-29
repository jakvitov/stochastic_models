library(expm)
source("basic_markov_operations.R")

#Return matrix with TRUE at given index if the given element is recurrent
IsRecurrent <- function(P) {
  res = rep(TRUE, nrow(P))
  rm = availability_matrix(P)
  #print(rm)
  for(row in 1:nrow(rm)) {
    for(col in 1:ncol(rm)) {
      if (rm[row, col] == TRUE && rm[col, row] != TRUE && row != col) {
        res[row] = FALSE
      }
    }
  }
  return(res)
}

test_matrix = matrix(c(0, 0.1, 0.9, 0, 0,  0.1, 0, 0.9, 0, 0,   0, 0, 0, 0.5, 0.5,   0, 0, 0, 0.9, 0.1,   0, 0, 0, 0.1, 0.9), nrow=5, ncol=5, byrow=TRUE)
exp_res = c(FALSE, FALSE, FALSE, TRUE, TRUE)

IsRecurrent(test_matrix)
print(IsRecurrent(test_matrix) == exp_res)
print(IsRecurrent(test_matrix))
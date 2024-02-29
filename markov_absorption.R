#This file includes functions regarding absorptive markov chains
source("basic_markov_operations.R")

#Return TRUE if given matrix representing markov chain (adjacency matrix) is absorptive
is_absorptive = function(m_chain) {
  av_mat = availability_matrix(m_chain)
  for (i in 1:nrow(m_chain)) {
    #Absorptive node
    if (m_chain[i, i] == 1) {
      #Now we check, if we can reach this node from every other one
      valid = TRUE
      for (j in 1:nrow(m_chain)) {
        #We cannot reach current node from the node j and j is not absorptive
        if (av_mat[j, i] == FALSE && !(m_chain[j,j] == 1)){
          valid = FALSE
          break
        }
      }
      if (valid) {
        return(TRUE)
      }
    }
  }
  return(FALSE)
}

test_abs_mat = matrix(c(0,0.5,0.5, 0, 1, 0, 00, 0, 1), nrow=3, ncol=3, byrow=TRUE)
test_non_abs_mat = matrix(c(0, 0.5, 0.5, 0,  0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0), nrow=4, ncol=4, byrow=TRUE)
print(is_absorptive(test_abs_mat))
print(is_absorptive(test_non_abs_mat))
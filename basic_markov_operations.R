

#Return availability matrix for the given markov chain represented by 
#adjacency matrix
availability_matrix = function(m_chain) {
  res = m_chain
  n = nrow(m_chain)
  #We already have m_chain to 1, since res = m_chain initialization
  for (i in range(2:(n-1))) {
    res = res + (m_chain)^i
  }
  return(res != 0)
}

test_m_chain = matrix(c(0.1,0.5,0.4, 0.0, 0.8, 0.2, 0.0, 1.0, 0.0), nrow=3, ncol=3, byrow=TRUE)
print(availability_matrix(test_m_chain))
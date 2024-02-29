

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

#Given a matrix representing a markov chain, return a list of vectors including all
#communication classes 
#Example list(c(1,2,3),c(4,5))
communication_classes = function(m_chain) {
  #Get the availability matrix - core of this algorithm
  av_mat = availability_matrix(m_chain)
  res = list()
  assigned = c()
  #Loop over the rows (nodes) and find all, that are reachable and backwards (communicate)
  for (row in 1:nrow(m_chain)){
    current = c()
    for (col in row:ncol(m_chain)) {
      #We can reach there and back again - we communicate - add the given col to current vector
      if ((row != col) && (av_mat[row,col]==TRUE) && (av_mat[col,row])==TRUE){
        current = c(current, row, col)
        assigned = c(assigned, row, col)
      }
    }
    # The current row does not communicate with anyone and was not yet assigned
    # The length is obsolete, but we speed it up by using less of %in% comparisons
    if ((length(current) == 0) && !(row %in% assigned)) {
      current = c(row)
    }
    res[[row]] = c(current)
  }
  return(res)
}

test_m_chain = matrix(c(0.1,0.5,0.4, 0.0, 0.8, 0.2, 0.0, 1.0, 0.0), nrow=3, ncol=3, byrow=TRUE)
print(availability_matrix(test_m_chain))
print(communication_classes(test_m_chain))
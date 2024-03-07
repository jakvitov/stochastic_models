library("expm")
#This file contains scripts regarding the chains regularity

#Return true if the given chain is regular
is_regular_pw = function(chain)  {
  s = nrow(chain)
  #Higher than this all elements of the matrix must be non zero
  n = s^2-2*s+1
  res = chain%^%n;
  return(all.equal(res,matrix(0, nrow(chain), ncol(chain))))
}

test_chain = matrix(c(0, 0.5, 0.5, 0.25, 0.5, 0.25, 0.25, 0.25, 0.5), nrow =3, ncol=3, byrow=TRUE)
print(is_regular_pw(test_chain))
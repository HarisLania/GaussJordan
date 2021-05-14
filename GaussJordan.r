A <- matrix(c(-8,1,-2,-20,2,-6,-1,-38,-3,-1,7,-34), 3, 4, byrow = T)
GaussJordan <- function(A) {
  n <- nrow(A)
  m <- ncol(A)
  for(i in 1:n) {
    A[i,] <- A[i, ]/A[i,i]
    for(j in 1:i-1){
      A[j,i:m] <- A[j,i:m] - (A[j,i] * A[i, i:m])
    }
    if (i==n) {
      break
    }
    for(j in i+1:(n-i)){
      A[j,i:m] <- A[j,i:m] - (A[j,i] * A[i, i:m])
    }
  }
  return(A)
}

GaussBackward <- function(A) {
  n <- nrow(A)
  m <- ncol(A)
  print(A)
  for(i in 1:n) {
    x <- A[i,m]
    print(x)
  }
}

##GaussBackward(GaussJordan(A))
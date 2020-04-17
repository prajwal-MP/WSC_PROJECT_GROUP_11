#input : mat: ordering matrix
#output : optimised ranking
BordaRanking <- function(mat){
  
  N <- ncol(mat)
  Value <- vector(length = N)
  names(Value) <- as.vector(mat[1,])
  for(i in seq(nrow(mat))){
    for(j in seq(ncol(mat))){
      Value[mat[i,j]] <- N - j + 1
      
    }
  }
  return(names(sort( Value,decreasing=TRUE)))
  
}

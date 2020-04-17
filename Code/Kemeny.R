#input : mat - matrix conatining orderering
#	n - number of columns
# 	v - numver of row or vertex names
#sco function: return kendall tau distance between rankings
#
#output:
# 	returns local optimised kemeny ranking 
sco<- function(mat1,v, n){
  scor <- 0
  for(i in seq(n)){
    scor <- scor + kendall.tau(mat1[v,],mat1[i,])
  }
  return(scor)
}
kem <- function(mat,n,v){
  #converting order to rank
  #print(mat)
  #mat = order2rank(mat,TO = ,TC = )
  #print(mat)
  #mat <- rank2order(mat,items = colnames(mat))
  #print(mat)
  mat = order2rank(mat,TO = ,TC = )
  
  KScore <- sco(mat,v,n)
  tempmat <- mat
  n <- ncol(mat)
  cou <- -1
  while(cou !=0){
    cou <- 0
    tempmat <- mat
 # for(i in seq(n-1)){
    for(j in seq(n-1)){
      t <- tempmat[v,j]
      tempmat[v,j] <- tempmat[v,j+1]
      tempmat[v,j+1] <- t
      
      if(sco(tempmat,v,nrow(mat)) < sco(mat,v,nrow(mat))){
        mat <- tempmat
        cou <- cou + 1
      }
      else{
        tempmat <- mat
      }
   # }
  }
}
  #coll <- colnames(mat)
  #for(i in seq(ncol(mat))){
  #  mat[v,i] <- paste('C',as.character(mat[v,i]),sep='')
  #}
  mat <- rank2order(mat,items = colnames(mat))
  return(rev(mat[v,]))
}

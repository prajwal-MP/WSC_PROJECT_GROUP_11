##Input : mat : square matrix
#        delta : threshold
#        labels : vertex names
#Output : an epsilon threshold graph g
epsilon_threshold_graph <- function(mat,delta,labels=NULL){
  
  if(nrow(mat)!=ncol(mat)){
    stop("matrix is not a square matrix")

  }
  else if((delta<= 0) || (delta >=1)){
    stop("delta value should be in between  0 and 1")

  }
  else{
    
    if(is.null(labels)||(length(labels)!=nrow(mat)))
    labels = c(as.character(V(g)))
    g <- graph.empty(directed = FALSE)
    g <- add.vertices(g,nv = nrow(mat))
    g <- set.vertex.attribute(g,name="name", index = V(g), value=labels)
    if(nrow(mat)!=1)
    {
      for(i in 1:(ncol(mat)-1)){
      for(j in ((i+1) : ncol(mat))){
          if(mat[i,j]>=delta)
            g <- add.edges(graph = g,edges = c(i,j),weight=mat[i,j])
        
      }
    }
    }
    return(g)
  }
}

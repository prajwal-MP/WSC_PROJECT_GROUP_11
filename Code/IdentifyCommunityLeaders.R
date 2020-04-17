#input g : graph
#leaders: leader initial
#centrality : function to calculate centrality
#delta : threshold value
#output : a list leaders
IdentifyCommunityLeaders <- function(g,leaders,centrality,delta){
  mat <- as.matrix(centrality(g,leaders))
  subgra <- epsilon_threshold_graph(mat,delta,as.vector(leaders))
  LeaderList <- list()
  for(I in decompose.graph(subgra)){
    LeaderList <- c(LeaderList, list(V(I)$name))
  }
  return(LeaderList)
}
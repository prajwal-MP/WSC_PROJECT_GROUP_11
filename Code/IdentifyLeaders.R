#Input: graph : graph 
#centrality : function to calculate centrality
#sigma : threshold value
#Output :returns a graph with leaders nodes marked as leaders: attribute leader<-True
  

IdentifyLeaders <- function(graph,centrality, sigma){
  leader_nodes <- vector()
  graph <- set.vertex.attribute(graph,name="Centrality",index = V(graph),value = centrality(graph))
  for(v in get.vertex.attribute(graph, name="name")){
    taux =  0
    for(neighbor in neighbors(graph = graph,v = as.character(v))){
      if(get.vertex.attribute(graph = graph,name = "Centrality",index = neighbor) > get.vertex.attribute(graph = graph,name = "Centrality",index = as.character(v))){
        taux =  taux + 1.0
      }
    }
    if((taux/(1+length(neighbors)))>sigma)
      graph <- set.vertex.attribute(graph,name="leader",index = as.character(v),value = TRUE)
    else
      graph <- set.vertex.attribute(graph,name="leader",index = as.character(v),value = FALSE)
  }
  return(as.vector(names(V(graph)[which(V(graph)$leader==TRUE)])))
}


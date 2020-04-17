#input
#       graph : input igraph 
#       sigma : threshold value for leader
#       delta : threshold value for community leader
#       centrality : functions such as betweenness, degree etc
#Output
#   returns a community associated with nodes
licod <- function(graph, sigma=0.8, centrality=betweenness ,delta=0.6){
  
  
  net  <- graph

  net <- set.vertex.attribute(net,"name", index=V(net),value = c(as.character(V(net))))
  
  
  if(is.connected(net)&&(length(get.vertex.attribute(net,"name"))==vcount(net))&&(vcount(net)>3)){
    
    #identify leaders
    nodes_leader  <- IdentifyLeaders(net,centrality, sigma)
    if(length(nodes_leader)==0){
      stop("No node leaders found")
    }
    
    #group leaders that share more than delta neighbors
    
    communitiesLeader <- IdentifyCommunityLeaders(net,nodes_leader,similarity,delta)
    strleaders <- vector()
    for(i in seq(length(communitiesLeader)))
      strleaders <- c(strleaders,paste("C",i,sep=""))
    names(communitiesLeader) <- strleaders
    vertex_names <-  V(net)$name
    
    
    old_com <- as.integer(c(vertex_names))
    names(old_com) <- vertex_names
    
    dis <- distances(net)
    rownames(dis) <- vertex_names
    colnames(dis) <- vertex_names
    
    for(v in vertex_names){
      tmp <- vector(length=length(communitiesLeader))
      names(tmp) <- strleaders
      
      for(leader in strleaders)
        tmp[leader] <- min(dis[v,communitiesLeader[[leader]]])
      net <- set.vertex.attribute(net,name = "membership",index = v,value = list(c(names(sort(tmp)))))
    }
    print(net)
    for(v in vertex_names){
      print(get.vertex.attribute(net,"membership",index=v))
    }
    for(v in vertex_names){
      mv <- matrix(nrow = (length(neighbors(net,v))+1), ncol = length(communitiesLeader))
      mv[1,] <- get.vertex.attribute(net,name = "membership",index = v)[[1]]
      nv_index <- 2
      for(nv in neighbors(net,v)){
        mv[nv_index,] <- get.vertex.attribute(net,name = "membership",index = nv)[[1]]
        nv_index <- nv_index +  1
      }
      
      net <- set.vertex.attribute(net,name = "new_membership",index = v,value = list(c(BordaRanking(mat = mv))))
    }
    
    net <- set.vertex.attribute(net,name = "membership",index = vertex_names,value = list(get.vertex.attribute(net,name = "new_membership",index = vertex_names))[[1]])
    
    
    community <- vector(length = length(vertex_names))
    names(community) <- vertex_names
    print(net)
    for(v in vertex_names){
      community[v] <- as.integer(strsplit(get.vertex.attribute(net,"membership",index=v)[[1]][1],"C")[[1]][2])
    }
    print(unique(community))
    print(community)
    for(l in 1:length(unique(community))){
      community[which(community == unique(community)[l])] <- l
    }
    com2memb <- create.communities(graph = net, membership = community,algorithm="Licod")
    return(com2memb)
  }
  else if(!is.connected(net)){
    # apply Licod algorithm on each connected subgraph
    com <- list()
    for(clust in groups(clusters(net))){
      if(length(clust)<3)
        com <- c(com,list(clust))
      com <- c(com, groups(licod(induced.subgraph(net,clust), sigma, centrality, delta)))
    }
    return(com)
  }
  else if(vcount(net)<=3){
    #each vertex is a community
    com <- list()
    for(v in V(net)$name)
      com <- c(com, list(as.integer(v)))
    
    return(com)
  }
}

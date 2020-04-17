library(ConsRank)
library(igraph)
library(VGAM)
library(rgl)
options(warn=-1)
source('epsilon_threshold_graph.R')
source('IdentifyLeaders.R')
source('IdentifyCommunityLeaders.R')
source('BordaRanking.R')
source('licod.R')

g1 <- graph.famous("Zachary")
GT1<-c(0,0,0,0,0,0,0,0,1,1,0,0,0,0,1,1,0,0,1,0,1,0,1,1,1,1,1,1,1,1,1,1,1,1)
wt <- licod(g1)
wt$membership
V(g1)$color <- wt$membership+1
g1$layout <- layout.fruchterman.reingold
plot(g1, vertex.label=NA)
NMI<-compare(wt$membership,GT1,method="nmi")
ARI<-compare(wt$membership,GT1,method="adjusted.rand")
print(NMI)
print(ARI)
print("--------------------------------------------")

g2<-read_graph("football.gml",format = "gml")
GT2<-get.vertex.attribute(g2,"value")
wt <- licod(g2)
wt$membership
V(g2)$color <- wt$membership+1
g2$layout <- layout.fruchterman.reingold
plot(g2, vertex.label=NA)
NMI<-compare(wt$membership,GT2,method="nmi")
ARI<-compare(wt$membership,GT2,method="adjusted.rand")
print(NMI)
print(ARI)
print("--------------------------------------------")

g3<-read_graph("polbooks.gml",format = "gml")
GT3<-get.vertex.attribute(g3,"value")
wt <- licod(g3)
wt$membership
V(g3)$color <- wt$membership+1
g3$layout <- layout.fruchterman.reingold
plot(g3, vertex.label=NA)
NMI<-compare(wt$membership,GT3,method="nmi")
ARI<-compare(wt$membership,GT3,method="adjusted.rand")
print(NMI)
print(ARI)
print("--------------------------------------------")

g4<-read_graph("dolphins.gml",format = "gml")
GT4<-list(1,0,1,1,1,0,0,0,1,0,1,1,1,0,1,1,1,0,1,0,1,1,0,1,1,0,0,0,1,1,1,0,0,1,1,1,1,1,1,1,1,0,1,1,1,1,1,1,0,1,1,1,1,1,0,1,0,0,1,1,0,1)
wt <- licod(g4)
wt$membership
V(g4)$color <- wt$membership+1
g4$layout <- layout.fruchterman.reingold
plot(g4, vertex.label=NA)
NMI<-compare(wt$membership,GT4,method="nmi")
ARI<-compare(wt$membership,GT4,method="adjusted.rand")
print(NMI)
print(ARI)
print("--------------------------------------------")

Gr <- list(g1,g2,g3,g4)
GTL <- list(GT1,GT2,GT3,GT4)
for (i in seq(4)){
print(paste("--------graph ",i,"---------"))
print("newmann")
newman<-cluster_edge_betweenness(Gr[[i]])
print(length(unique(newman$membership)))
print(modularity(newman))
NMI=compare(newman$membership,GTL[[i]],method="nmi")
ARI=compare(newman$membership,GTL[[i]],method="adjusted.rand")
print(NMI)
print(ARI)

print("Louvian")
louvain<-cluster_louvain(Gr[[i]])
print(length(unique(louvain$membership)))
print(modularity(louvain))
NMI=compare(louvain$membership,GTL[[i]],method="nmi")
ARI=compare(louvain$membership,GTL[[i]],method="adjusted.rand")
print(NMI)
print(ARI)

print("Walktrap")
walktrap<-walktrap.community(Gr[[i]],modularity = TRUE)
print(length(unique(walktrap$membership)))
print(modularity(walktrap))
NMI=compare(walktrap$membership,GTL[[i]],method="nmi")
ARI=compare(walktrap$membership,GTL[[i]],method="adjusted.rand")
print(NMI)
print(ARI)

}
    

library(philentropy)
library(igraph)
source("rng_graph.R")
PRINT <- function(Di){
  gh <- rng_graph(Di,similarity = FALSE)
  gh <- as.undirected(gh,mode = "each")
  print(paste("#edges:",gsize(gh)))
  print(paste("diameter : ",diameter(gh)))
  print(paste("average degree : ", mean(degree(gh))))
  print(paste("density : ", edge_density(gh)))
  print(paste("transitivity : ", transitivity(gh,type = "localaverage")))
}
g <- iris
levels(g[5][,1]) <- c(1,2,3)
g[5][,1] <- as.numeric(levels(g[5][,1]))[g[5][,1]]
Di <- distance(g,method="euclidean")
PRINT(Di)

Di <- distance(g,method="chebyshev")
PRINT(Di)

Di <- distance(g,method="cosine")
Di <- 1 - Di
PRINT(Di)

print ("------------------------------------------")

g <- read.csv('glass.data',header=FALSE)
g <- g[2:11]
Di <- distance(g,method="euclidean")
PRINT(Di)

Di <- distance(g,method="chebyshev")
PRINT(Di)

Di <- distance(g,method="cosine")
Di <- 1 - Di
PRINT(Di)

print ("------------------------------------------")

g <- read.csv('wine.data',header=FALSE)
Di <- distance(g,method="euclidean")
PRINT(Di)

Di <- distance(g,method="chebyshev")
PRINT(Di)

Di <- distance(g,method="cosine")
Di <- 1 - Di
PRINT(Di)

print ("------------------------------------------")

g <- read.csv('vehicle.dat',sep='\t',header=FALSE)
levels(g[19][,1]) <- c(1,2,3,4)
g[19][,1] <- as.numeric(levels(g[19][,1]))[g[19][,1]]
Di <- distance(g,method="euclidean")
PRINT(Di)

Di <- distance(g,method="chebyshev")
PRINT(Di)

Di <- distance(g,method="cosine")
Di <- 1 - Di
PRINT(Di)


print ("------------------------------------------")

g <- read.csv('abalone.data',header=FALSE)
levels(g[1][,1]) <- c(1,2,3)
g[1][,1] <- as.numeric(levels(g[1][,1]))[g[1][,1]]
Di <- distance(g,method="euclidean")
PRINT(Di)

Di <- distance(g,method="chebyshev")
PRINT(Di)

Di <- distance(g,method="cosine")
Di <- 1 - Di
PRINT(Di)



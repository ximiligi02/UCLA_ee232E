library("igraph")

##################################
##########  problem 6   ##########
##################################

g = read.graph(file = "facebook_combined.txt", directed = FALSE)

#finding core nodes in the graph
coreNodesIndex = numeric(0)
for (i in 1 : vcount(g)) {
  if (length(neighbors(g, i)) > 200) {
    coreNodesIndex = c(coreNodesIndex, i)
  }
}
# print(coreNodesIndex)

# type 1:
index_degreeMax = numeric(0);
index_clustering_coefMax = numeric(0);
index_densityMax = numeric(0);

degreeMax = numeric(0);
clustering_coefMax = numeric(0);
densityMax = numeric(0);
# type2:
index_degreeMin = numeric(0);
index_clustering_coefMin = numeric(0);
index_densityMin = numeric(0);

degreeMin = numeric(0);
clustering_coefMin = numeric(0);
densityMin = numeric(0);

for(i in 1:length(coreNodesIndex)) {
  coreNeighbors =neighbors(g,coreNodesIndex[i])
  corePersonal = induced.subgraph(g,c(coreNodesIndex[i],coreNeighbors))
  coreCommunity = walktrap.community(corePersonal)
  communityLarge = numeric(0)
  
  for(j in 1:length(coreCommunity)) {
    number = V(corePersonal)[which(coreCommunity$membership==j)]
    if (length(number)>10) {
      communityLarge = c(communityLarge,j)
    }
  }
  degreeAverage = numeric(0)
  clustering_coeficient = numeric(0)
  densityAverage = numeric(0)
  
  for(k in 1:length(communityLarge)) {
    communityStruct = induced.subgraph(corePersonal, V(corePersonal)[which(coreCommunity$membership==communityLarge[k])])
    degreeAverage = c(degreeAverage,mean(degree(communityStruct))/vcount(communityStruct))
    clustering_coeficient = c(clustering_coeficient,transitivity(communityStruct,type="global"))
    densityAverage = c(densityAverage,graph.density(communityStruct))
  }
  # type 1
  index_degreeMax=c(index_degreeMax,communityLarge[which.max(degreeAverage)])
  index_clustering_coefMax = c(index_clustering_coefMax,communityLarge[which.max(clustering_coeficient)])
  index_densityMax=c(index_densityMax,communityLarge[which.max(densityAverage)])
  # type 2
  index_degreeMin=c(index_degreeMin,communityLarge[which.min(degreeAverage)])
  index_clustering_coefMin = c(index_clustering_coefMin,communityLarge[which.min(clustering_coeficient)])
  index_densityMin=c(index_densityMin,communityLarge[which.min(densityAverage)])
  
  cat("Max: ")
  cat("degreeAverage",communityLarge[which.max(degreeAverage)]," ")
  cat("cluster_coefficient",communityLarge[which.max(clustering_coeficient)]," ")
  cat("density",communityLarge[which.max(densityAverage)],"\n")
  cat("Min: ")
  cat("degreeAverage",communityLarge[which.min(degreeAverage)]," ")
  cat("cluster_coefficient",communityLarge[which.min(clustering_coeficient)]," ")
  cat("density",communityLarge[which.min(densityAverage)],"\n")
  
  degreeMax = c(degreeMax,max(degreeAverage))
  clustering_coefMax = c(clustering_coefMax,max(communityLarge))
  densityMax = c(densityMax,max(densityAverage))
  degreeMin = c(degreeMin,min(degreeAverage))
  clustering_coefMin = c(clustering_coefMin,min(communityLarge))
  densityMin = c(densityMin,min(densityAverage))
}

index_degreeMax = t(data.matrix(index_degreeMax))
index_clustering_coefMax = t(data.matrix(index_clustering_coefMax))
index_densityMax = t(data.matrix(index_densityMax))

degreeMax = t(data.matrix(degreeMax))
clustering_coefMax = t(data.matrix(clustering_coefMax))
densityMax = t(data.matrix(densityMax))

index_degreeMin = t(data.matrix(index_degreeMin))
index_clustering_coefMin = t(data.matrix(index_clustering_coefMin))
index_densityMin = t(data.matrix(index_densityMin))

degreeMin = t(data.matrix(degreeMin))
clustering_coefMin = t(data.matrix(clustering_coefMin))
densityMin = t(data.matrix(densityMin))

plot(degreeMax)
plot(clustering_coefMax)
plot(densityMax)









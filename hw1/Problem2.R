#loading igraph
library("igraph")

#problem 2
#part(a)
nodesNum <- 1000
fat_tailed_graph <- barabasi.game(n = nodesNum, directed = FALSE)
deg <- degree.distribution(fat_tailed_graph)
plot(deg, type = "p", main = "Degree Distribution for Fat-tailed Distribution",xlab="degree",ylab="frequency")

#find the diameter of the graph
dia = numeric(0)
for (i in 1 : 50) {
  fat_tailed_graph <- barabasi.game(n = nodesNum, directed = FALSE)
  dia = c(dia, diameter(fat_tailed_graph))
}
avgDia = mean(dia) 
cat("Diameter of Fat Tail Distribution Graph : ", avgDia)

#part(b)
calculateModularity <- function(fat_tailed_graph) {
  clusters <- clusters(fat_tailed_graph)
  index_GCC <- which.max(clusters$csize)
  non_GCC_nodes <- (1:vcount(fat_tailed_graph))[clusters$membership != index_GCC]
  GCC <- delete.vertices(fat_tailed_graph, non_GCC_nodes)
  community_GCC <- fastgreedy.community(GCC)
  modularity_GCC <- modularity(community_GCC)
  return (modularity_GCC)
}

cat("Fat Tail Distribution Graph is connected :", is.connected(fat_tailed_graph))
mod = calculateModularity(fat_tailed_graph)
cat("Modularity of the Fat-tailed Graph is : ", mod)


#part(c)
large_graph <- barabasi.game(n = 10000, directed = FALSE)
large_mod = calculateModularity(large_graph)
cat("Modularity of the larger Fat-tailed Graph is : ", large_mod)

#part(d)
deg = numeric(0)
for (i in 1 : 100) {
  randNum = sample(1000, 1)
  nei = neighbors(fat_tailed_graph, V(fat_tailed_graph)[randNum], mode = 1)
  neiNode = sample(nei, 1)
  deg = c(deg, degree(fat_tailed_graph, neiNode))
}
plot(density(deg), type = "h", main='Degree distribution of Random Neighbor', xlab='degree', ylab='density')
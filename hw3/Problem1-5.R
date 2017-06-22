#loading package
library("igraph")

###################################
#             Problem 1           #
###################################

#reading the data and create network
myData = read.table("sorted_directed_net.txt")
g = graph.data.frame(myData, directed = TRUE)
vcount(g)
ecount(g)
#checking if the network is connected
is.connected(g)

#giant connected component
graphs = decompose.graph(g)
gccIndex = which.max(sapply(graphs, vcount))
gcc = graphs[[gccIndex]]
#plot(gcc, main = "Greatest Connected Component")


###################################
#             Problem 2           #
###################################
#plot the in&out degree distribution of the largest connected components
plot(degree.distribution(gcc, mode = "in"), type = 'h', main = "In-degree Distribution of gcc")
plot(degree.distribution(gcc, mode = "out"), type = 'h', main = "Out-degree Distribution of gcc")


###################################
#             Problem 3           #
###################################
#measure community structure using fastgreedy & labal.paropagation method
#option 1, number of edges unchanged, and just remove the directions.
g1 = as.undirected(gcc, mode = "each")
struct1 = label.propagation.community(g1)
cat("Option 1: Modularity of community using label.propagation algorithm is: ", modularity(struct1))

#option 2, merge two mutual directed edges into one
sqrtWeight<-function(weight){
  result = sqrt(weight[1]*weight[2])
  result
}
g2 = as.undirected(gcc, mode = "collapse", edge.attr.comb = sqrtWeight)
struct2 = fastgreedy.community(g2) #com_fg
struct3 = label.propagation.community(g2) #com_lp
cat("Option 2: Modularity of community using fastgreedy algorithm is: ", modularity(struct2))
cat("Option 2: Modularity of community using label.propagation algorithm is: ", modularity(struct3))

###################################
#             Problem 4           #
###################################
x = which.max(sizes(struct2))
subg = induced.subgraph(g2, which(membership(struct2) == x))
vcount(subg)
#fastgreedy algorithm
subStruct = fastgreedy.community(subg)
cat("Modularity of subcommunity using fastgreedy algorithm is: ", modularity(subStruct))
print(sizes(subStruct))
#label.propogation algorithm
subStruct = label.propagation.community(subg)
cat("Modularity of subcommunity using label.propogation algorithm is: ", modularity(subStruct))
print(sizes(subStruct))

###################################
#             Problem 5           #
###################################
index = which(sizes(struct2)>100)
for(i in 1:length(index)) {
  cat("The index of the sub-community is: ", index[[i]], "\n")
  nonStruct = induced.subgraph(g2, which(membership(struct2) == index[i]))
  vcount(nonStruct)
  nonStruct_100up = fastgreedy.community(nonStruct)
  cat("Modularity of subcommunity using fastgreedy algorithm is: ", modularity(nonStruct_100up), "\n")
  print(sizes(nonStruct_100up))
  nonStruct2_100up = label.propagation.community(nonStruct)
  cat("Modularity of subcommunity using label propagation algorithm is: ", modularity(nonStruct2_100up), "\n")
  print(sizes(nonStruct2_100up))
}
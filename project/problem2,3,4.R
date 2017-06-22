library("igraph")

#read and open file
g<-read.graph("facebook_combined.txt",format="ncol",directed=FALSE)

#problem2
sub_g1 = induced.subgraph(g, c(1, neighbors(g,1)))
vertex_vector = rep(4,vcount(sub_g1))
vertex_vector[1]=6
vertex_color = rep("skyblue1",vcount(sub_g1))
vertex_color[1] ="black"
plot.igraph(sub_g1,vertex.size=vertex_vector,vertex.label=NA,vertex.color=vertex_color)
num_node = vcount(sub_g1)
num_edge = ecount(sub_g1)

#problem3
core_index = numeric(0)
core_degree = numeric(0)

#find the core nodes
for(i in 1: vcount(g)){
  if(length(neighbors(g,i))>200){
    core_index = c(core_index, i)
    core_degree = c(core_degree, length(neighbors(g,i)))
  }
}

core_ave_degree = mean(core_degree)
print(core_index)
print(core_ave_degree)

#fastgreedy
fastgreedy = fastgreedy.community(sub_g1)
color_vector = fastgreedy$membership+1
t=fastgreedy$membership+1
vertex_vector = rep(4,vcount(sub_g1))
vertex_vector[1]=6
print(modularity(fastgreedy))
plot(fastgreedy,sub_g1,vertex.color=color_vector,vertex.label=NA,vertex.size=vertex_vector)

#edge-betweenness
edgebetween = edge.betweenness.community(sub_g1)
color_vector = edgebetween$membership+1
print(modularity(edgebetween))
plot(sub_g1,vertex.color=color_vector,vertex.label=NA,vertex.size=vertex_vector)

#Infomap
infomap = infomap.community(sub_g1)
color_vector = infomap$membership+1
print(modularity(infomap))
plot(sub_g1,vertex.color=color_vector,vertex.label=NA,vertex.size=vertex_vector)

#histograms
hist(fastgreedy$membership, col="red", main="Community structure using Fast-Greedy Algorithm", xlab="Community number", ylab="Number of nodes")
hist(edgebetween$membership, col="blue", main="Community structure using Edge-Betweenness Algorithm", xlab="Community number", ylab="Number of nodes")
hist(infomap$membership,col="yellow",main="Community structure using Infomap Algorithm",xlab="Community number",ylab="Number of nodes")

#problem4
sub_g1wo = induced.subgraph(g,neighbors(g,1))

fastgreedy_r = fastgreedy.community(sub_g1wo)
color_vector = fastgreedy_r$membership+1
plot(sub_g1wo,vertex.color=color_vector, vertex.label=NA, vertex.size=4)

edgebetween_r = edge.betweenness.community(sub_g1wo)
color_vector = edgebetween_r$membership+1
plot(sub_g1wo, vertex.color=color_vector,vertex.label=NA, vertex.size=4)

infomap_r = infomap.community(sub_g1wo)
color_vector = infomap_r$membership+1
plot(sub_g1wo, vertex.color=color_vector,vertex.label=NA, vertex.size=4)

hist(fastgreedy_r$membership, col="red", main="Community structure using Fast-Greedy Algorithm", xlab="Community number", ylab="Number of nodes")
hist(edgebetween_r$membership, col="blue", main="Community structure using Edge-Betweenness Algorithm", xlab="Community number", ylab="Number of nodes")
hist(infomap_r$membership,col="yellow",main="Community structure using Infomap Algorithm",xlab="Community number",ylab="Number of nodes")

print(modularity(fastgreedy_r))
print(modularity(edgebetween_r))
print(modularity(infomap_r))



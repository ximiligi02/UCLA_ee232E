library("igraph")
library("netrw")

g<-read.graph("/Users/Administrator/Desktop/sorted_directed_net.txt",format="ncol",directed=TRUE)

#find the gcc
cl<-clusters(g)
gccID<-which.max(cl$csize)
nonGccID <-(1:vcount(g))[cl$membership != gccID]
gcc<-delete.vertices(g,nonGccID) 

#find the communities by fast greedy algorithm as done in problem 3
gcc_U<-as.undirected(gcc,mode="collapse", edge.attr.comb=list(weight="prod"))
E(gcc_U)$weight<-sqrt(E(gcc_U)$weight)
fgc<-fastgreedy.community(gcc_U) 


#initiate m for each node of which the length is the number of communities
m <- matrix(0,vcount(gcc),length(sizes(fgc)))
for(i in 1:vcount(gcc))
{
  m[i,fgc$membership[i]]=1
}

#initiate M for each node of which the length is the number of communities
M <- matrix(0,vcount(gcc),length(sizes(fgc)))

#calculate vj and M for each node
for(i in 1:vcount(gcc))
{
  randWalk<-netrw(gcc,1000,start.node<-i,
                  damping=0.85, T=100, 
                  output.walk.path=TRUE, output.walkers=0:999,
                  output.visit.prob=TRUE, output.nodes=0:(vcount(gcc)-1),
                  output.device="memory")
 visitProb<-randWalk$ave.visit.prob
 sortVP<-sort(visitProb,index.return=TRUE,decreasing=TRUE)
 for(j in 1:30)
 {
   M[i,]=M[i,]+sortVP$x[j]*m[sortVP$ix[j],]
 }
}

#find the second largest membership value for each node.
second_largest <- matrix(0,vcount(gcc),1)
for(i in 1:vcount(gcc))
{
  second_largest[i,1] = sort(M[i,], decreasing = TRUE)[2]
}

#The distribution of the second largest membership value
hist(second_largest, xlab = "Second largest membership value", ylab = "Frequency")

#set a threshold and find all the nodes whose second_largest membership value is larger than it
THR <- 0.02
filtered <- list() 
num_filtered <- 0 
for(i in 1:vcount(gcc))
{
  if (second_largest[i,1] >= THR)
  {
    filtered<- rbind(filtered,i)
    num_filtered<-num_filtered+1
  }
}

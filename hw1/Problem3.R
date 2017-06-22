#loading library
library("igraph")
#problem 3
#part(a)
g <- aging.prefatt.game(1000, pa.exp=1, aging.exp=-1, aging.bin=1000, directed=FALSE)
plot(degree.distribution(g),type="b", main = "Degree Distribution",xlab="degree",ylab="density")

#part(b)
community = fastgreedy.community(g)
mod = modularity(community)
cat("Modularity of the Preferential Attachment Model Graph is : ", mod)
plot(community, g)
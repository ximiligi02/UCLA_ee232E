#loading library
library("igraph")

#problem 4
#part(a)
nodesNum = 1000
dia = numeric(0)
for(i in 1 : 50) {
  g = forest.fire.game(nodesNum, fw.prob=0.37, bw.factor=0.32/0.37)
  dia = c(dia, diameter(g))
}
par(mfrow=c(1,2))
plot(degree.distribution(g,mode="in"),type="b",main="In-Degree Distirbution",xlab="In-degree",ylab="Density")
plot(degree.distribution(g,mode="out"),type="b",main="Out-Degree Distirbution",xlab="Out-degree",ylab="Density")

#part(b)
avgDia = mean(dia)
cat("Diameter of the forest fire model Graph is: ", avgDia)

#part(c)
community = spinglass.community(g)
mod = modularity(community)
cat("Modularity of the forest fire model is : ", mod)
plot(community, g)
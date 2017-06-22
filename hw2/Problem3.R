#loading packages
library("igraph")
library("netrw")

#part(a)

plotRelation = function(g, nodesNum, df) {
  #calculate degree for the network
  deg = numeric(0)
  deg = degree(g)
  
  #random walk
  rw = netrw(g, damping = df, T = 1000, output.walk.path = TRUE, output.visit.prob = TRUE)
  
  probSum = numeric(max(deg))
  count = numeric(max(deg))
  
  #plot the relaitionship between probability and degree
  for (i in 1:1000) {
    probSum[deg[i]] = probSum[deg[i]] + rw$ave.visit.prob[i]
    count[deg[i]] = count[deg[i]] + 1
  }
  plotprob = numeric(0)
  num = numeric(0)
  for (j in 1:max(deg))
  {
    if(count[j] != 0){
      probSum[j] = probSum[j]/count[j]
      plotprob = c(plotprob,probSum[j])
      num = c(num,j)
    }
  }
  plot(num, plotprob, type="o", main="Relationship Between Prob and Degree",xlab="Degree",ylab="Probability")
  corr = cor(num, plotprob)
  cat("The correlation between degree and prob is :", corr)
}

#generate network
nodesNum = 1000
prob = 0.01
g = random.graph.game(nodesNum, prob, directed = FALSE)
plotRelation(g, nodesNum, 1)

#part(b)
g_directed = random.graph.game(nodesNum, prob, directed = TRUE)
plotRelation(g_directed, nodesNum, 1)

#part(c)
plotRelation(g, nodesNum, 0.85)


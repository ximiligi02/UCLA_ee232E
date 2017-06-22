library("igraph")
library("netrw")

# part(a)
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

nodesNum = 1000
prob = 0.01
g_directed2 = random.graph.game(nodesNum, prob, directed = TRUE)

plotRelation(g_directed2, nodesNum, 0.85)
ranking=page.rank(g_directed2)
plot(ranking$vector,main="PageRank",xlab="Pages",ylab="rankscore")

# part(b)
plotRelation2 = function(g, nodesNum, df, teleportation){
  deg = numeric(0)
  deg = degree(g)
  rw = netrw(g, damping = df, teleport.prob = teleportation, output.visit.prob = TRUE)
  probSum = numeric(max(deg) - min(deg) + 1)
  count = numeric(max(deg) - min(deg) + 1)
  
  for (i in 1:1000) {
    probSum[deg[i]] = probSum[deg[i]] + rw$ave.visit.prob[i]
    count[deg[i]] = count[deg[i]] + 1
  }
  
  plotprob = numeric(0)
  num = numeric(0)
  for (j in 1:max(deg)) {
    probSum[j] = probSum[j]/count[j]
    plotprob = c(plotprob,probSum[j])
    num = c(num,j)
  }
  plot(num, plotprob, type="o", main="Relationship Between Prob and Degree",xlab="Degree",ylab="Probability")
  corr = cor(num, plotprob)
  cat("The correlation between degree and prob is :", corr)
}

g2 = random.graph.game(nodesNum, prob, directed = FALSE)
plotRelation2(g_directed2, nodesNum, 0.85, ranking$vector)
pageRanking = personalized.pagerank(g2, damping = 0.85, prob = ranking$vector)
plot(pageRanking, main = "Personalized_PageRank", xlab="Pages",ylab="rankscore")

# part(c)
modified = rep(1/nodesNum,nodesNum)
plotRelation2(g_directed2,nodesNum,0.85,modified)
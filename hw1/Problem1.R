library ("igraph")

# Problem 1
# part(a)
prob = c(0.1, 0.05, 0.01)
nodesNum = 1000

g1 <- erdos.renyi.game(nodesNum, prob[1], type="gnp", directed = FALSE)
g2 <- erdos.renyi.game(nodesNum, prob[2], type="gnp", directed = FALSE)
g3 <- erdos.renyi.game(nodesNum, prob[3], type="gnp", directed = FALSE)

dg1 <- degree.distribution(g1)
dg2 <- degree.distribution(g2)
dg3 <- degree.distribution(g3)


layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE))
plot(dg1,type="h", main = "Degree Distribution with p=0.1 ",xlab="degree",ylab="density")
plot(dg2,type="h", main = "Degree Distribution with p=0.05",xlab="degree",ylab="density")
plot(dg3,type="h", main = "Degree Distribution with p=0.01",xlab="degree",ylab="density")

# part(b)
con1 = con2 = con3 = dia1 = dia2 = dia3 = numeric(0);
for (i in 1:50) {
  g1 <- erdos.renyi.game(nodesNum, prob[1], type="gnp", directed = FALSE)
  g2 <- erdos.renyi.game(nodesNum, prob[2], type="gnp", directed = FALSE)
  g3 <- erdos.renyi.game(nodesNum, prob[3], type="gnp", directed = FALSE)
  con1 = c(con1, is.connected(g1))
  con2 = c(con2, is.connected(g2))
  con3 = c(con3, is.connected(g3))
  dia1 = c(dia1, diameter(g1))
  dia2 = c(dia2, diameter(g2))
  dia3 = c(dia3, diameter(g3))
}
avgCon1 <- mean(con1)
avgCon2 <- mean(con2)
avgCon3 <- mean(con3)
avgDia1 <- mean(dia1)
avgDia2 <- mean(dia2)
avgDia3 <- mean(dia3)

# part(c) 
# use bineary search to find the target pc
pc = numeric(0)
for (i in 1:50) {
  left = 0
  right = prob[3] #0.01
  step = 0.0001
  while(TRUE){
    curPc = left + (right - left) / 2
    gsmall = erdos.renyi.game(nodesNum, (curPc - step), type="gnp", directed = FALSE)
    glarge = erdos.renyi.game(nodesNum, (curPc + step), type="gnp", directed = FALSE)
    if (!is.connected(gsmall) && is.connected(glarge)) {
      pc = c(pc, curPc)
      break
    }
    else if (is.connected(gsmall)) {
      right = curPc
    }
    else {
      left = curPc
    }
  }
}
avgPc <- mean(pc)
cat("The threshold probability for this Graph is: ", avgPc)
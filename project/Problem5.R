#loading package
library("igraph")

##################################
##########  problem 5   ##########
##################################

#Define functions

calcEmd <- function(coreNode, neiNode, g) {
  emd = length(intersect(neighbors(g, neiNode), neighbors(g, coreNode)))
  emd
}

calcDisp <- function(coreNode, neiNode, g, arr) {
  #generate the desired network for dispersion calc
  mutualFriends = intersect(neighbors(g, coreNode), neighbors(g, neiNode))
  dispSubgraph = delete.vertices(g, c(coreNode, neiNode))
  distance = numeric(0)
  len = length(mutualFriends)
  #arr = array(0:0, c(len, len))
  for (m in 1 : len) {
    for (n in (m + 1) : len) {
      if(n <= 0 || m > len) {
        next
      }
      mi = mutualFriends[m]
      ni = mutualFriends[n]
      if (is.na(ni)) {
        next
      }
      print("########")
      print(mi)
      print(ni)
      if(arr[mi,ni] == 0) {
        arr[mi,ni] = shortest.paths(dispSubgraph, which(V(dispSubgraph) == mi), which(V(dispSubgraph) == ni))
      }
      distance = c(distance, arr[mi,ni])
    }
  }
  sum(distance)
}

#calculate the embeddedness and dispersion respectively, and plot the distribution
calculateAndPlot <- function(g, coreNode) {

  maxDisp = 0
  maxDispNode = 0  
  maxEmd = 0
  maxEmdNode = 0
  maxRate = 0
  maxRateNode = 0
  
  curNeighbors = neighbors(g, coreNode)
  arr = array(0:0, c(4039, 4039))
  
  for (neiNode in curNeighbors) {
    print("**********")
    print(neiNode)
    print("**********")
    emd = calcEmd(coreNode, neiNode, g)
    disp = calcDisp(coreNode, neiNode, g, arr)
    
    if (emd > maxEmd) {
      maxEmd = emd
      maxEmdNode = neiNode
    }
    
    if (disp > maxDisp) {
      maxDisp = disp
      maxDispNode = neiNode
    }
    
    if (emd > 0 && disp/emd > maxRate) {
      maxRate = disp/emd
      maxRateNode = neiNode
    }
  }

  neighborsGraph = delete.vertices(g, which(!( (1:vcount(g)) %in% curNeighbors)))

  if (maxDispNode > 0) {
    struct = fastgreedy.community(neighborsGraph)
    mem = membership(struct)

    sizeVet = rep(3, vcount(neighborsGraph))
    sizeVet[maxDispNode] = 8
    colEd = rep(8, ecount(neighborsGraph))
    colEd[which(get.edgelist(neighborsGraph,name=F)[,1] == maxDispNode | get.edgelist(neighborsGraph,name=F)[,2] == maxDispNode)] = 3
    E(neighborsGraph)$color = colEd
    widEd = rep(1, ecount(neighborsGraph))
    widEd[which(get.edgelist(neighborsGraph, name=F)[,1] == maxDispNode | get.edgelist(neighborsGraph,name=F)[,2] == maxDispNode)] = 3
    dev.new();
    plot(neighborsGraph, vertex.label=NA, vertex.color=mem,vertex.size=sizeVet, edge.width = widEd,mark.groups = by(seq_along(mem), mem, invisible), main="Max dispersion");
  } else {
    cat("No legal max dispersion node")
  }

  if (maxEmdNode > 0) {
    struct = fastgreedy.community(neighborsGraph)
    mem = membership(struct)

    sizeVet = rep(3, vcount(neighborsGraph))
    sizeVet[maxEmdNode] = 8
    colEd = rep(8, ecount(neighborsGraph))
    colEd[which(get.edgelist(neighborsGraph,name=F)[,1] == maxEmdNode | get.edgelist(neighborsGraph,name=F)[,2] == maxEmdNode)] = 3
    E(neighborsGraph)$color = colEd
    widEd = rep(1, ecount(neighborsGraph))
    widEd[which(get.edgelist(neighborsGraph,name=F)[,1] == maxEmdNode | get.edgelist(neighborsGraph,name=F)[,2] == maxEmdNode)] = 3
    dev.new();
    plot(neighborsGraph, vertex.label=NA, vertex.color=mem,vertex.size=sizeVet, edge.width = widEd,mark.groups = by(seq_along(mem), mem, invisible),main="Max embeddedness");
  } else {
    cat("No legal max embeddedness node")
  }

  if (maxRateNode > 0) {
    struct = fastgreedy.community(neighborsGraph)
    mem = membership(struct)

    sizeVet = rep(3, vcount(neighborsGraph))
    sizeVet[maxRateNode] = 8
    colEd = rep(8, ecount(neighborsGraph))
    colEd[which(get.edgelist(neighborsGraph,name=F)[,1] == maxRateNode | get.edgelist(neighborsGraph,name=F)[,2] == maxRateNode)] = 3
    E(neighborsGraph)$color = colEd
    widEd = rep(1, ecount(neighborsGraph))
    widEd[which(get.edgelist(neighborsGraph, name=F)[,1] == maxRateNode | get.edgelist(neighborsGraph,name=F)[,2] == maxRateNode)] = 3
    dev.new();
    plot(neighborsGraph, vertex.label= NA, vertex.color=mem,vertex.size=sizeVet, edge.width = widEd,mark.groups = by(seq_along(mem), mem, invisible),main="Max dispersion/embeddedness rate");
  } else {
    cat("No legal max rate node")
  }
}

calcEmdDispDistribution <- function(g, coreNodes) {
  emdVec = numeric(0)
  dispVec = numeric(0)
  arr = array(0:0, c(4039, 4039))
  for (coreNode in coreNodes) {
  
    curNeighbors = neighbors(g, coreNode)

    for (neiNode in curNeighbors) {
      emd = calcEmd(coreNode, neiNode, g)
      disp = calcDisp(coreNode, neiNode, g, arr)
      emdVec = c(emdVec, emd)
      dispVec = c(dispVec, disp)
    }
  }
  
  hist (emdVec, breaks=seq (-0.5, by=1, length.out=max(emdVec) + 2), main = "Embeddedness Distribution", xlab = "Embeddedness", ylab = "Frequency")
  hist (dispVec, breaks=seq (-0.5, by=1, length.out=max(dispVec) + 2), main = "Dispersion Distribution", xlab = "Dispersion", ylab = "Frequence")
  
}

########### test start ###############
########### test end ###############

########### main funtion ###########

g = read.graph(file = "facebook_combined.txt", directed = FALSE)

#finding core nodes in the graph
coreNodesIndex = numeric(0)
for (i in 1 : vcount(g)) {
  if (length(neighbors(g, i)) > 200) {
    coreNodesIndex = c(coreNodesIndex, i)
  }
}
print(coreNodesIndex)

calcEmdDispDistribution(g, coreNodesIndex)

#randomly picked 3 core nodes with index 1, 10, 30

calculateAndPlot(g, coreNodesIndex[1])
calculateAndPlot(g, coreNodesIndex[10])
calculateAndPlot(g, coreNodesIndex[15])

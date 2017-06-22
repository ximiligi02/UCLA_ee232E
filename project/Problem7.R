#loading package
library("igraph")

filePath = "gplus/"
egos = dir(filePath, pattern = "circles")
egoId = list()
userNum = 0

for (i in 1 : length(egos)) {
  
  egoId[[i]] = strsplit(egos[i], ".circles")
  edgeListPath = paste(filePath, egoId[[i]], ".edges", sep = "")
  circlesPath = paste(filePath, egoId[[i]], ".circles", sep = "")
  
  openCircle = file(circlesPath, open = "r")
  
  linesInCircle = readLines(openCircle)
  close(openCircle)
  
  #find the users with more than 2 circles
  circlesNum = length(linesInCircle)
  if (circlesNum > 2) {
    print("The ID of the user with more than 2 circles is:")
    print(i)
    print("with number of circles = ")
    print(circlesNum)
    
    circles = list()
    edgeNew = c()
    userNum = userNum + 1
    
    g = read.graph(edgeListPath, format = "ncol", directed = TRUE)
    
    for (j in 1 : circlesNum) {
      temp = strsplit(linesInCircle[j], "\t")
      circles[[j]] = temp[[1]][-1]
    }
    gNew = add.vertices(g, 1, name=egoId[[i]])
    for (vidx in 1:(vcount(gNew) - 1)) {
      edgeNew = c(edgeNew, c(vcount(gNew), vidx))
    }
    gNew = add.edges(gNew, edgeNew)
    
    wStruct = walktrap.community(gNew)
    iStruct = infomap.community(gNew)
    plot(wStruct,gNew,vertex.label=NA,vertex.size=7,edge.arrow.size=0.2,main="Community Structure of No.7 using Walktrap")
    plot(iStruct,gNew,vertex.label=NA,vertex.size=7,edge.arrow.size=0.2,main="Community Structure of No.7 using Infomap")
    
    for (m in 1 : max(wStruct$membership)) {
      node = vector()
      for (n in 1 : length(wStruct$membership)) {
        if (wStruct$membership[n] == m) {
          node = c(node, (wStruct$name[n]))
        }
      }
      overlap = vector()
      for (n in 1 : circlesNum) {
        common = intersect(node, circles[[n]])
        tmp = length(common) / length(node)
        overlap = c(overlap, tmp)
      }
      print("The overlap is:")
      print(overlap)
    }
  }
}

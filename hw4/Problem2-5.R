
library('igraph')
library('readr')
library('xts')

############## Problem 2 ##############

token = read.csv("f:/finance_data/finance_data/Name_sector.csv",stringsAsFactors = FALSE) 

cnames = token$Symbol

MAT = matrix(list(),length(cnames),2)

P.data <- data.frame(A = character(),
                 B = character(), 
                 C = numeric(), 
                 stringsAsFactors=FALSE) 

colnames(P.data) = c("Node 1", "Node 2", "weights")


row_count = 0

for(i in cnames) {                                       #loop through each company name

  row_count = row_count + 1
  cat("Processing processing" , row_count , "out of", length(cnames),"\n")
  zhongjie = paste('f:/finance_data/finance_data/data/',i,'.csv',sep = "")                  #get directory of current company name
  cd = read.csv(zhongjie ,stringsAsFactors = FALSE)$Close                        #get all closing data
  
  ri = numeric(0)                                                                    
  
  for (j in 2:length(cd)){
    ri[j-1] = log(cd[j]) - log(cd[j-1])  
  }
    
  MAT[[row_count,1]] = i
  MAT[[row_count,2]] = ri
  }







calculateDijMatrix = function(tickerNames, logReturnMatrix, ddij) {
  
  #initializing
  totalNum = 1;
  count = 1;
  DijMatrix = data.frame(A = character(), B = character(), C = numeric(), stringsAsFactors=FALSE);
  colnames(DijMatrix) = c("V1", "V2", "weights");
  fanhui=list();
  #calculating matrix
  for (node1 in 1 : (length(tickerNames) - 1)) {
    cat("Constructing Dij Matrix", count, "out of", length(tickerNames) - 1, "\n");
    count = count + 1;
    cat("node1:", node1, "\n");
    
    for (node2 in (node1+1) : length(tickerNames)) {
      ri = logReturnMatrix[[node1, 2]];
      rj = logReturnMatrix[[node2, 2]];
      meanRi = mean(ri);
      meanRj = mean(rj);
      
      pij = (mean(ri*rj) - meanRi*meanRj)/sqrt((mean(ri^2) - meanRi^2)*(mean(rj^2) - meanRj^2));
      dij = sqrt(2*(1-pij));
      ddij = c(ddij , dij);
      DijMatrix[totalNum, 1] = tickerNames[node1];
      DijMatrix[totalNum, 2] = tickerNames[node2];
      DijMatrix[totalNum, 3] = dij;
      totalNum = totalNum + 1;
    }
  }
  fanhui=list(DijMatrix, ddij);
  return(fanhui);
}

dij = numeric()
ppp=calculateDijMatrix(cnames, MAT, dij);



g1 = graph.data.frame(ppp[[1]],directed = FALSE)

cat("Number of nodes in the network: ",length(V(g1)),"\n")
cat("Number of edges in the network: ",length(E(g1)),"\n")

 hist( x= ppp[[2]], breaks = seq(from = min(ppp[[2]]), to = max(ppp[[2]]), by = (max(ppp[[2]])-min(ppp[[2]]))/50), 
  main = "Histogram of d_ij's", xlab = "d_ij Value", ylab = "Frequency")




############## Problem 3 ##############


nodes = V(g1)$name                            # vector of all nodes
sectors = token$Sector                        # vector of all sectors 
u_sectors = unique(sectors)

node_cols = rep(0,length(nodes))
col_id = 1
for(i in u_sectors){
  node_cols[which(sectors == i)] = col_id     #identify colors to unique sectors
  col_id = col_id + 1
}

g1_mst = mst(g1 , weights = ppp[[1]]$weights)   # create MST

cat("Number of nodes in the network: ",length(V(g1_mst)),"\n") # as a sanity check
cat("Number of edges in the network: ",length(E(g1_mst)),"\n")

plot(g1, vertex.color = node_cols, 
      vertex.size = rep(7,length(nodes)),
      vertex.label = NA,
      main = "Correlations Graph" ) # correlation graph

plot(g1_mst, vertex.color = node_cols ,
     vertex.size = rep(7,length(nodes)) , 
     vertex.label = NA, 
     main = "Minimal Spanning Tree" ) # mst


############## Problem 4 ##############


nei = numeric()
random_sector = numeric()
for(i in u_sectors){
  u_sec_nodes = nodes[which(sectors == i)]                 # all nodes in sector
  tot_prob = 0
  for(j in u_sec_nodes){
    temp_neigh = as.vector(neighbors(g1_mst , j , mode = "all"))              # neighbors node IDs
    j_node_id = which(j == nodes)                                             # find id of jth node
    num_same_sec = length(which(sectors[temp_neigh] == sectors[j_node_id]))   # number of neighbors in same sector
    temp_prob = num_same_sec / length(temp_neigh)                             # ratio of neighbors in the same sector vs tot neighbors
    tot_prob = tot_prob + temp_prob 
  }
  nei = c(nei , tot_prob/length(u_sec_nodes))
  random_sector = c(random_sector,length(u_sec_nodes)/length(nodes))          #ratio of num nodes in sector i vs total nodes in graph 
}

sector_clustering_results = cbind(nei,random_sector)      # VIEW THIS VARIABLE IN THE ENVIRONMENT TO SEE RESULTS
rownames(sector_clustering_results) = u_sectors
colnames(sector_clustering_results) = c("nei","Random Sector")



############## Problem 5 ##############

finding=function(i,j){
if(i>j){
temp=i;
i=j;
j=temp;
}
a=0;
jia=504;
fanhui=0;
if(i>1){
for(ii in 1:i-1){
fanhui=fanhui+jia;
jia=jia-1;
}
fanhui=fanhui-jia-1;
}
fanhui=fanhui+j-i;
return(fanhui);
}


jiebao=1;
for (vertex1 in 1:503) {
  for (vertex2 in (vertex1+1):504) {
    for (vertex3 in (vertex2+1):505) {
        print(jiebao);
jiebao=jiebao+1;
        if (ppp[[2]][finding(vertex1,vertex2)] > (ppp[[2]][finding(vertex1,vertex3)] + ppp[[2]][finding(vertex2,vertex3)])) 
          print("FALSE")
       
    }
  }
}


g1_mst_dir = as.directed(g1_mst, 'mutual')
g1_double_mst = as.undirected(g1_mst_dir, 'each' )

el = get.edgelist(g1_double_mst)
student = as.vector(E(g1_double_mst)$weights)
out = cbind(el, student)

#Exporting the graph to python to fine Euler Tour
write.csv(out, file = "f:/project_2_data/double_mst.csv",row.names=FALSE)

#Read in the results from python
tsp_python = read.table('f:/project_2_data/tsp.txt')
tsp_path_names = as.vector(tsp_python[,1])

node_names = V(g1)$name
tsp_path_idx = numeric()
for(i in 1:length(tsp_path_names)){
  matches = which(tsp_path_names[i] == node_names)
  tsp_path_idx = c(tsp_path_idx,matches)
}

adjacency_matrix = get.adjacency(g1, attr = 'weights',sparse = FALSE, names = FALSE)

#Calculate weight of travelling salesman path
tsp_weight = 0
for(i in 2:length(tsp_path_idx)){
  tsp_weight = tsp_weight + adjacency_matrix[tsp_path_idx[i],tsp_path_idx[i-1]]
}

lower_bound = sum(E(g1_mst)$weights)
upper_bound = sum(E(g1_double_mst)$weights)

cat("Sum of weights of min spanning tree: ",lower_bound,"\n")
cat("Sum of weights of double min spanning tree: ",upper_bound,"\n")
cat("Sum of edge weights of TSP solution: ", tsp_weight,"\n")




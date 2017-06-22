########################################
############## Question 6 ##############
########################################
#loading package
library("igraph")
#library("readr")

findClosingPrices = function(ticker) {
  data = read.csv(paste("finance_data/", "data/", ticker, ".csv", sep=""));
  return (data$Close);
}

findMondayClosingPrices = function(ticker) {
  data = read.csv(paste("finance_data/", "data/", ticker, ".csv", sep=""));
  dataDate = data$Date;
  dataClosingPrices = data$Close;
  monday_idx = which(weekdays(as.Date(dataDate,'%Y-%m-%d')) == "Monday");
  return(dataClosingPrices[monday_idx]);
}

#arg1: tickerNames --> all ticker names collected in Name_sector.csv
#arg2: mod --> mod == general: calculate all log return merix
#              mod == monday: sample the data on Monday and calculate the log return metric
#@return: log return matrix
calculateLogReturnMatrix = function(tickerNames, mod="general") {
  matrix = matrix(list(), length(tickerNames), 2);
  count = 1;
  for (ticker in tickerNames) {
    if (mod == "general") {
      closingPrices = findClosingPrices(ticker);
    } else if(mod == "Monday") {
      closingPrices = findMondayClosingPrices(ticker);
    } else {
      cat("No such mod in calculateLogReturnMatrix function\n");
    }
    cat("Constructing logReturnMatrix...", count, "out of", length(tickerNames),"\n");
    logReturnList = numeric(0);
    for (day in 2:length(closingPrices)) {
      logReturnList[day - 1] = log(closingPrices[day]) - log(closingPrices[day - 1]);
    }
    matrix[[count, 1]] = ticker;
    matrix[[count, 2]] = logReturnList;
    count = count + 1;
  }
  return (matrix);
}

#arg1: tickerNames --> all ticker names collected in Name_sector.csv
#arg2: logReturnMatrix --> log return value of all tickers
#@return: dij matrix, where dij = sqrt(2*(1-Pij))
calculateDijMatrix = function(tickerNames, logReturnMatrix) {
  
  #initializing
  totalNum = 1;
  count = 1;
  DijMatrix = data.frame(A = character(), B = character(), C = numeric(), stringsAsFactors=FALSE);
  colnames(DijMatrix) = c("V1", "V2", "weights");
  
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
      
      DijMatrix[totalNum, 1] = tickerNames[node1];
      DijMatrix[totalNum, 2] = tickerNames[node2];
      DijMatrix[totalNum, 3] = dij;
      totalNum = totalNum + 1;
    }
  }
  return(DijMatrix);
}

filePath = "finance_data/Name_sector.csv";
tickerFile = read.csv(filePath, stringsAsFactors = FALSE);
tickerNames = tickerFile$Symbol;

logReturnMatrix = calculateLogReturnMatrix(tickerNames, "Monday");
DijMatrix = calculateDijMatrix(tickerNames, logReturnMatrix)

########################################
############## Question 7 ##############
########################################
#loading package
library("igraph")
#setwd("~/Documents/EE232E/HW4")
#library("readr")

findClosingPrices = function(ticker) {
  data = read.csv(paste("finance_data/", "data/", ticker, ".csv", sep=""));
  return (data$Close);
}

findMondayClosingPrices = function(ticker) {
  data = read.csv(paste("finance_data/", "data/", ticker, ".csv", sep=""));
  dataDate = data$Date;
  dataClosingPrices = data$Close;
  monday_idx = which(weekdays(as.Date(dataDate,'%Y-%m-%d')) == "Monday");
  return(dataClosingPrices[monday_idx]);
}

#arg1: tickerNames --> all ticker names collected in Name_sector.csv
#arg2: mod --> mod == general: calculate all log return merix
#              mod == monday: sample the data on Monday and calculate the log return metric
#@return: log return matrix
calculateLogReturnMatrix = function(tickerNames, mod="general") {
  matrix = matrix(list(), length(tickerNames), 2);
  count = 1;
  for (ticker in tickerNames) {
    if (mod == "general") {
      closingPrices = findClosingPrices(ticker);
    } else if(mod == "Monday") {
      closingPrices = findMondayClosingPrices(ticker);
    } else {
      cat("No such mod in calculateLogReturnMatrix function\n");
    }
    cat("Constructing logReturnMatrix...", count, "out of", length(tickerNames),"\n");
    logReturnList = numeric(0);
    for (day in 2:length(closingPrices)) {
      logReturnList[day - 1] = log(closingPrices[day]) - log(closingPrices[day - 1]);
    }
    matrix[[count, 1]] = ticker;
    matrix[[count, 2]] = logReturnList;
    count = count + 1;
  }
  return (matrix);
}

#arg1: tickerNames --> all ticker names collected in Name_sector.csv
#arg2: logReturnMatrix --> log return value of all tickers
#@return: dij matrix, where dij = sqrt(2*(1-Pij))
calculateDijMatrix_modify = function(tickerNames, logReturnMatrix) {
  
  #initializing
  totalNum = 1;
  count = 1;
  DijMatrix = data.frame(A = character(), B = character(), C = numeric(), stringsAsFactors=FALSE);
  colnames(DijMatrix) = c("V1", "V2", "weights");
  
  Pij = numeric()
  Dij = numeric()
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
      
      # original P
      Pij = c(Pij, pij);
      
      # modified p
      pij[which(pij > 0.3)] = -1;
      
      # modified d
      dij = sqrt(2*(1-pij));
      
      # modified d
      Dij = c(Dij, dij);
      
      DijMatrix[totalNum, 1] = tickerNames[node1];
      DijMatrix[totalNum, 2] = tickerNames[node2];
      DijMatrix[totalNum, 3] = dij;
      totalNum = totalNum + 1;
    }
  }
  newList <- list("matrix" = DijMatrix, "P" = Pij, "D" = Dij)
  return(newList);
}

filePath = "finance_data/Name_sector.csv";
tickerFile = read.csv(filePath, stringsAsFactors = FALSE);
tickerNames = tickerFile$Symbol;

logReturnMatrix = calculateLogReturnMatrix(tickerNames, "general");
DijMatrix_list = calculateDijMatrix_modify(tickerNames, logReturnMatrix)
DijMatrix_modify = DijMatrix_list$matrix
g1 = graph.data.frame(DijMatrix_modify,directed = FALSE)

nodes = V(g1)$name # vector of all nodes
sectors = tickerFile$Sector # vector of all sectors 
uniqueSectors = unique(sectors)

node_cols = rep(0,length(nodes))
col_id = 1
for(i in uniqueSectors){
  node_cols[which(sectors == i)] = col_id #identify colors to unique sectors
  col_id = col_id + 1
}

g1_mst = mst(g1 , weights = DijMatrix_modify$weights) # create minimal spanning tree

plot(g1_mst, vertex.color = node_cols, vertex.size = rep(7,length(nodes)), vertex.label = NA, main = "Modified Minimal Spanning Tree" ) # mst

# Hist of un-modified P's 
hist( x= DijMatrix_list$P, breaks = seq(from = min(DijMatrix_list$P), to = max(DijMatrix_list$P), by = (max(DijMatrix_list$P)-min(DijMatrix_list$P))/50), 
      main = "Histogram of Un-modified P_ij's", xlab = "d_ij Value", ylab = "Frequency")

# Hist of D calculated from modified P
hist( x= DijMatrix_list$D, breaks = seq(from = min(DijMatrix_list$D), to = max(DijMatrix_list$D), by = (max(DijMatrix_list$D)-min(DijMatrix_list$D))/50), 
      main = "Histogram of Modified d_ij's ", xlab = "d_ij Value", ylab = "Frequency")

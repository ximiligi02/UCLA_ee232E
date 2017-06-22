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
DijMatrix = calculateDijMatrix(tickerNames, logReturnMatrix);

g = graph.data.frame(DijMatrix, directed = FALSE);

sectors = tickerFile$Sector;
uniqueSectors = unique(sectors);
colors = rep(0, length(tickerNames));
colorId = 1
for (curSector in uniqueSectors) {
  colors[which(sectors == curSector)] = colorId;
  colorId = colorId + 1;
}

mst = mst(g, weights = DijMatrix$weights);

weightList = numeric(0);
count = 1;

for (weight in DijMatrix$weights) {
  cat("constructing weightList", count, "out of", length(DijMatrix$weights), "\n");
  count = count + 1;
  
  if (is.na(weight)) {
    next;
  }
  weightList = c(weightList, weight);
}

hist(x = weightList, breaks = seq(from = min(weightList), to = max(weightList), by = (max(weightList)-min(weightList))/40), 
      main = "Histogram of dij's (Using only Monday Data)", xlab = "dij", ylab = "Distribution");

plot(mst, vertex.color = colors, vertex.size = 6, vertex.label = NA, 
     main = "Minimal Spanning Tree (Using only Monday Data)")


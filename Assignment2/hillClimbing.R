source("logLin.R")
source("cliques.R")

gm.search <- function(table,initGraph) {
  gm.search_(table,initGraph)
}

gm.search_ <- function(table,graph) {
  
  quality <- detQuality(table,graph)
  
  neighbors <- findAllNeighbors(graph)
  
  if(length(neighbors) > 0) {
    neighborQualities <- lapply(neighbors,function(x) detQuality(table,x))
    bNeighborIndex <- minimumNeighbor(neighborQualities)
    bNeighbor <- neighbors[[bNeighborIndex]]
    bNeighborQuality <- neighborQualities[[bNeighborIndex]]
    
    if(quality > bNeighborQuality) {
       return (gm.search_(table,bNeighbor))
    }
  }
  
  list(quality,table,graph)
}

detQuality <- function(table,graph) {
  cliques <- getCliques(graph)
  lglin <- loglin(table,cliques,print=FALSE)
  dev <- lglin$lrt
  df <- lglin$df
  
  nParams <- length(table) - df
  #dev + 2 * nParams
  dev + log(sum(table)) * nParams
}

findAllNeighbors <- function(graph) {
  size <- nrow(graph) * ncol(graph) - nrow(graph)
  l <- vector("list",size)
  k <- 1
  i <- 1
  j <- 1
  iLength <- ncol(graph)
  jLength <- nrow(graph)
  
  while(i <= iLength) {
    
    while(j <= jLength) {
      
      if(i != j) {
        newGraph <- graph
        newGraph[j,i] <- inverse(newGraph[j,i])
        l[[k]] <- newGraph
        k <- k + 1
      }
      
      j <- j + 1
    }
    j <- 1
    i <-i + 1
  }
  
  l
}

getCliques <- function(graph) {
  cliques <- find.cliques(c(),1:6,c(),graph,list())
  post.process(cliques)
}

minimumNeighbor <- function(nQualities) {
  if(length(nQualities) <= 0) {
    NA
  }
  else {
    
    currQuality <- nQualities[[1]]
    i <- 1
    index <- 1
    
    while(i <= length(nQualities)) {
      if(nQualities[[i]] <= currQuality) {
        currQuality <- nQualities[[i]]
        index <- i
      }
      i <- i + 1
    }
    index
  }
}

inverse <- function(val) {
  if (val == 1) {
    0
  }
  else {
    1
  }
}
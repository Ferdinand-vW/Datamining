source("logLin.R")
source("cliques.R")

gm.restart <- function(nstart,prob,seed,table,forward,backward,score) {
  set.seed(seed)
  numAttrs <- length(dim(table))
  
  currModel <- gm.search(table,matrix(0,numAttrs,numAttrs))
  
  i <- 1
  while(i <= nstart) {
    
    graph <- generateGraph(prob,numAttrs)
    model <- gm.search(table,graph,forward,backward,score)
    
    if(model[[1]] <= currModel[[1]]) {
      currModel <- model
    }
    
    i <- i + 1
  }
  
  return (currModel)
  
}

generateGraph <- function(prob,numAttrs) {
  i <- 1
  j <- 1
  matrix <- matrix(0,numAttrs,numAttrs)
  while(i <= nrow(matrix)) {
    while(j <= ncol(matrix)) {
      if(i == j) {
        matrix[j,i] = 0
      }
      else {
        matrix[j,i] = sample(0:1,1)
      }
      
      j <- j + 1
    }
    
    i <- i + 1
    j <- 1
  }
  
  matrix
}

gm.search <- function(table,graph,forward,backward,score) {
  
  cliques <-  getCliques(graph)
  quality <- detQuality(table,cliques,score)
  currModel <- list("",graph)
  steps <-  c()
  searching <- TRUE
  
  while(searching) {
    
    neighbors <- findAllNeighbors(currModel[[2]],forward,backward)
    if(length(neighbors) > 0) {
      nbrCliques <- lapply(neighbors, function(x) getCliques(x[[2]]))
      nbrQualities <- lapply(nbrCliques, function(x) detQuality(table,x,score))
      bNeighborIndex <- minimumNeighbor(nbrQualities)
      bNeighbor <- neighbors[[bNeighborIndex]]
      bNeighborQuality <- nbrQualities[[bNeighborIndex]]
      bCliques <- nbrCliques[[bNeighborIndex]]
      bNeighbor <- list(paste(bNeighbor[[1]],"(score = ", bNeighborQuality, ")"),bNeighbor[[2]])
      
      if(quality > bNeighborQuality) {
        print(quality)
        print(bNeighborQuality)
        print(quality - bNeighborQuality > 0)
        quality <- bNeighborQuality
        currModel <- bNeighbor
        cliques <- bCliques
        steps <- c(steps,bNeighbor[[1]])
      }
      else {
        searching <- FALSE
      }
    }
  }
  
  return (list(score = quality,model = currModel[[2]], cliques = cliques,trace = steps,call = match.call()))
}

detQuality <- function(table,cliques,score) {
  
  lglin <- loglin(table,cliques,print=FALSE)
  dev <- as.double(lglin$lrt)
  df <- lglin$df
  
  nParams <- length(table) - df
  
  if(score == "aic") {
    round(dev + 2 * nParams,2)
  }
  else if(score == "bic") {
    round(dev + log(sum(table)) * nParams,2)
  }
  else {
    stop(paste("use either bic or aic for the score. You used:",score))
  }
}

findAllNeighbors <- function(graph,forward,backward) {
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
        if(newGraph[j,i] == 0 && forward) {
          newGraph[j,i] <- 1
          l[[k]] <- list(paste("Added:",j,"to",i,sep=" "),newGraph)
          k <- k + 1
        }
        else if(newGraph[j,i] == 1 && backward) {
          newGraph[j,i] <- 0
          l[[k]] <- list(paste("Removed:",j,"to",i,sep=" "),newGraph)
          k <- k + 1
        }
      }
      
      j <- j + 1
    }
    j <- 1
    i <-i + 1
  }
  
  l
}

getCliques <- function(graph) {
  cliques <- find.cliques(c(),1:(nrow(graph)),c(),graph,list())
  return (post.process(cliques))
}

minimumNeighbor <- function(nQualities) {
  currQuality <- nQualities[[1]]
  i <- 2
  index <- 1
  
  while(i <= length(nQualities)) {
    if(nQualities[[i]] < currQuality) {
      currQuality <- nQualities[[i]]
      index <- i
    }
    i <- i + 1
  }
  index
}
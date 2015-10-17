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

gm.search <- function(table,initGraph,forward,backward,score) {
  res <- gm.search_(table,initGraph,forward,backward,score,c())
  list(score = res$score, cliques = res$cliques, model = res$model, trace = res$trace,call = match.call())
}

gm.search_ <- function(table,graph,forward,backward,score,steps) {
  cliques <- getCliques(graph)
  quality <- detQuality(table,cliques,score)
  
  neighbors <- findAllNeighbors(graph,forward,backward)
  
  if(length(neighbors) > 0) {
    neighborQualities <- lapply(neighbors,function(x) detQuality(table,getCliques(x[[2]]),score))
    bNeighborIndex <- minimumNeighbor(neighborQualities)
    bNeighbor <- neighbors[[bNeighborIndex]]
    bNeighborQuality <- neighborQualities[[bNeighborIndex]]
    bNeighbor <- list(paste(bNeighbor[[1]],"(score = ", bNeighborQuality, ")"),bNeighbor[[2]])
    print(bNeighbor)
    if(quality > bNeighborQuality) {
       return (gm.search_(table,bNeighbor[[2]],forward,backward,score,c(steps,bNeighbor[[1]])))
    }
  }
  
  list(score = quality,model = graph, cliques = cliques,trace = steps)
}

detQuality <- function(table,cliques,score) {
  
  lglin <- loglin(table,cliques,print=FALSE)
  dev <- lglin$lrt
  df <- lglin$df
  
  nParams <- length(table) - df
  
  if(score == "aic") {
    dev + 2 * nParams
  }
  else if(score == "bic") {
    dev + log(sum(table)) * nParams
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
  post.process(cliques)
}

minimumNeighbor <- function(nQualities) {
  currQuality <- nQualities[[1]]
  i <- 1
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
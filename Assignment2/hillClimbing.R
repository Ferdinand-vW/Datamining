source("logLin.R")
source("cliques.R")

gm.restart <- function(nstart,prob,seed,table,forward,backward,score) {
  set.seed(seed)
  numAttrs <- length(dim(table))
  
  #We'll start off with the saturated model
  currModel <- generateGraph(1,numAttrs)
  quality <- detQuality(table,currModel,score)
  
  i <- 1
  while(i <= nstart) {
    
    #graph <- matrix(sample(0:1,numAttrs * numAttrs,replace=TRUE,prob=c(1-prob,prob)),numAttrs,numAttrs)
    graph <- generateGraph(prob,10)
    model <- gm.search(table,graph,forward,backward,score)
    
    if(model[[1]] <= quality) {
      currModel <- model[[2]]
      quality <- model[[1]]
    }
    
    i <- i + 1
  }
  
  return (currModel,quality)
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
        matrix[j,i] = sample(0:1,1,prob=c(1-prob,prob))
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
  currModel <- graph
  prevModels <- list(graph)
  steps <-  c()
  searching <- TRUE
  
  while(searching) {
    
    neighborsAndSteps <- findAllNeighbors(currModel,forward,backward,prevModels)
    neighbors <- neighborsAndSteps[[1]]
    neighborSteps <- neighborsAndSteps[[2]]
    
    if(length(neighbors) > 0) {
      
      #get the cliques for all neighbors
      nbrCliques <- lapply(neighbors, 
            function(x){
              print(x)
              getCliques(x)
            })
      #calculate the quality  of all neighbors
      nbrQualities <- lapply(nbrCliques, 
            function(x) {
              q <- detQuality(table,x,score)
              print(q)
              q
            })
      #get the index of the neighbor with the best quality
      bNeighborIndex <- which.min(nbrQualities)
      
      #Get the model,cliques and quality of the best neighbor
      bNeighbor <- neighbors[[bNeighborIndex]]
      bNeighborQuality <- nbrQualities[[bNeighborIndex]]
      bCliques <- nbrCliques[[bNeighborIndex]]
      bSteps <- neighborSteps[[bNeighborIndex]]

      if(quality >= bNeighborQuality) {
        quality <- bNeighborQuality
        currModel <- bNeighbor
        prevModels <- c(prevModels,list(currModel))
        cliques <- bCliques
        steps <- c(steps,bSteps)
      }
      else {
        searching <- FALSE
      }
    }
  }
  
  return (list(score = quality,model = currModel, cliques = cliques,trace = steps,call = match.call()))
}

detQuality <- function(table,cliques,score) {
  lglin <- loglin(table,cliques,print=FALSE)
  dev <- lglin$lrt
  df <- lglin$df
  
  nParams <- length(table) - df
  
  if(score == "aic") {
    round(dev + 2 * nParams,2)
  }
  else if(score == "bic") {
    round(dev + log(sum(table)) * nParams,2)
  }
  else {
    stop(paste("use either bic or aic for the score. You used: ",score))
  }
}

findAllNeighbors <- function(graph,forward,backward,prevModels) {
  size <- nrow(graph) * ncol(graph) - nrow(graph)
  graphs <- list()
  steps <- list()
  i <- 1
  j <- 1
  iLength <- ncol(graph)
  jLength <- nrow(graph)
  print("test")
  print(graph)
  while(i <= iLength) {
    
    while(j <= jLength) {
      
      if(i != j) {
        newGraph <- graph
        if(newGraph[j,i] == 0 && forward) {
          newGraph[j,i] <- 1
          if(!containsMatrix(prevModels,newGraph)) {
            graphs <- c(graphs,list(newGraph))
            steps <- c(steps,list(paste("Added:",j,"to",i,sep=" ")))
          }

        }
        else if(newGraph[j,i] == 1 && backward) {
          newGraph[j,i] <- 0
          if(!containsMatrix(prevModels,newGraph)) {
            graphs <- c(graphs,list(newGraph))
            steps <- c(steps,list(paste("Removed:",j,"to",i,sep=" ")))
          }
        }
      }
      
      j <- j + 1
    }
    j <- 1
    i <-i + 1
  }
  
  list(graphs,steps)
}

getCliques <- function(graph) {
  cliques <- find.cliques(c(),1:(nrow(graph)),c(),graph,list())
  return (post.process(cliques))
}

containsMatrix <- function(listOf,matrix) {
  i <- 1
  while(i <= length(listOf)) {
    if(!identical(listOf[[i]],matrix)) {
      i <- i + 1
    }
    else {
      return(TRUE)
    }
  }
  
  return (FALSE)
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
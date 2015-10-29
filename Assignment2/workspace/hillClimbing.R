source("cliques.R")

#Function that can restart the hill-climbing algorithm a number of times
#with randomly generated initial models
gm.restart <- function(nstart,prob,seed,table,forward,backward,score) {
  set.seed(seed)
  numAttrs <- length(dim(table))
  
  #We'll start off with the saturated model
  currModel <- generateGraph(1,numAttrs)
  cliques <- getCliques(currModel)
  quality <- detQuality(table,cliques,score)
  res <- list()
  
  i <- 1
  while(i <= nstart) {
    
    #Generate a graph and then use it as an initial model for 
    #hill-climbing search
    graph <- generateGraph(prob,numAttrs)
    model <- gm.search(table,graph,forward,backward,score)
    
    #If we found a better model we update the current model
    if(model$score <= quality) {
      currModel <- model$model
      quality <- model$score
      res <- model
    }
    
    i <- i + 1
  }
  
  res
}

#Generates a graph given a probability and size
generateGraph <- function(prob,numAttrs) {
  i <- 1
  j <- 1
  matrix <- matrix(0,numAttrs,numAttrs)
  while(i <= nrow(matrix)) {
    while(j <= ncol(matrix) && j < i) {
      #Determine whether an edge should be added or not
      val <- sample(0:1,1,prob=c(1-prob,prob))
      matrix[j,i] <- val
      matrix[i,j] <- val
      j <- j + 1
    }
    
    i <- i + 1
    j <- 1
  }
  
  matrix
}

#Function that using a hill-climbing algorithm finds the best fit
#for a table
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
      nbrCliques <- lapply(neighbors, function(x) getCliques(x))
      #calculate the quality  of all neighbors
      nbrQualities <- lapply(nbrCliques, function(x) detQuality(table,x,score))
      #get the index of the neighbor with the best quality
      bNeighborIndex <- which.min(nbrQualities)
      
      #Get the model,cliques and quality of the best neighbor
      bNeighbor <- neighbors[[bNeighborIndex]]
      bNeighborQuality <- nbrQualities[[bNeighborIndex]]
      bCliques <- nbrCliques[[bNeighborIndex]]
      bSteps <- neighborSteps[[bNeighborIndex]]

      #If the quality of the best neighbor is at least as good
      #as the quality of the current model than we adopt the neighbor
      #as the current model and repeat the loop
      if(quality >= bNeighborQuality) {
        #Store some information about the best neighbor
        quality <- bNeighborQuality
        currModel <- bNeighbor
        prevModels <- c(prevModels,list(currModel))
        cliques <- bCliques
        steps <- c(steps,bSteps)
      }
      else {#We reached a local optimum, so we'll stop searching
        searching <- FALSE
      }
    }
  }
  
  return (list(score = quality,model = currModel, cliques = cliques,trace = steps,call = match.call()))
}

#Calculates the quality for a given model
detQuality <- function(table,cliques,score) {
  lglin <- loglin(table,cliques,print=FALSE)
  dev <- lglin$lrt
  df <- lglin$df
  
  # Nr of parameters of the table
  nParams <- length(table) - df
  
  #choose between using aic or bic for scoring
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

#finds all the neighbors of a given model
findAllNeighbors <- function(graph,forward,backward,prevModels) {
  graphs <- list()
  steps <- list()
  #row iterator
  i <- 1
  #column iterator
  j <- 1
  iLength <- ncol(graph)
  jLength <- nrow(graph)
  while(i <= iLength) {
    
    #We only need to loop over half the graph
    #If we change the value of [j,i] then we also change the 
    #value of [i,j] in the same way
    while(j <= jLength && j < i) {
  
      newGraph <- graph
      #Only change the value if we can
      if(newGraph[j,i] == 0 && forward) {
        newGraph[j,i] <- 1
        newGraph[i,j] <- 1
        #If this is a neighbor that we haven't seen before, then
        #we can add it as a neighbor
        if(!containsMatrix(prevModels,newGraph)) {
          graphs <- c(graphs,list(newGraph))
          steps <- c(steps,list(paste("Added:",j,"to",i,sep=" ")))
        }

      }#same as above but for removing edges
      else if(newGraph[j,i] == 1 && backward) {
        newGraph[j,i] <- 0
        newGraph[i,j] <- 0
        if(!containsMatrix(prevModels,newGraph)) {
          graphs <- c(graphs,list(newGraph))
          steps <- c(steps,list(paste("Removed:",j,"to",i,sep=" ")))
        }
      }
      #Go to the next column
      j <- j + 1
    }
    #Go to the next row and reset the column number
    j <- 1
    i <-i + 1
  }
  
  list(graphs,steps)
}

#Get the cliques using the function that was given
getCliques <- function(graph) {
  cliques <- find.cliques(c(),1:(nrow(graph)),c(),graph,list())
  return (post.process(cliques))
}

#Check whether a list of matrices contains a matrix
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
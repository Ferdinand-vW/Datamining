source("Tree.R")
source("Main.R")
source("Simplify.R")
source("Classify.R")

prepareData <- function(x) {
  index <- trainingIndex()
  data <- trainingdata(x,index)
  sample <- testsample(x,index)
  
  dataparts <- splitTrainingData(data)
  
  list(sample,dataparts)
}

evaluateSample <- function(sample,nmin,minleaf) {
  y <- sample[,"AHD"]
  x <- sample
  x[,"AHD"] <- NULL
  tree <- tree.grow(x,y,nmin,minleaf)
  tree.simplify(tree)
  cy <- tree.classify(tree,sample)
  
  error <- classificationError(y,cy)
  
  list(error,tree)
}

evaluateModel <- function(dataparts,nmin,minleaf) {
  
  res <- crossValidation(dataparts,nmin,minleaf)
  true <- res[[1]]
  pred <- res[[2]]
  classificationError(true,pred)
}

classificationError <- function(true,pred) {
  corrPred <- true[true==pred]
  
  numE <- length(pred) - length(corrPred)
  
  numE / length(true)
}

createTree <- function(dataparts) {
  y <- dataparts[[1]]
  x <- dataparts[-1]
  
  cx <- vapply(x,FUN=getClassLabels,numeric(20))
  cx <- (combineParts(cx))[[1]]
  x <- lapply(x,FUN=removeClassLabels)
  x <- combineParts(x)
  cy <- y[,"AHD"]
  #y[,"AHD"] <- NULL
  
  tree <- tree.grow(x,cx,25,4)
  tree <- tree.simplify(tree)
  predy <- tree.classify(tree,y)
  
  print(classificationError(cy,predy))
  tree
  
}



crossValidation <- function(parts,nmin,minleaf) {
  i <- 1
  size <- length(parts)
  
  results <- vector("list",2)
  
  while(i <= size) {
    
    single <- parts[[i]]
    others <- parts[-i]
    
    result <- classification(others,single,nmin,minleaf)
    results[[1]] <- c(results[[1]],result[[1]])
    results[[2]] <- c(results[[2]],result[[2]])
    
    i <- i + 1
  }
  
  results
}

trainingIndex <- function() {
  index <- sample(297,200)
}

partIndex <- function(size) {
  index <- sample(size,20)
}

trainingdata <- function(x,index) {
  x[index,]
}

testsample <- function(x,index) {
  x[-index,]
}

splitTrainingData <- function(x) {
  l <- vector("list",10)
  j <- 1
  while(length(x[,1]) > 0) {
    i <- partIndex(length(x[,1]))
    part <- x[i,]
    #l <- list(l,part)
    l[[j]] <- part
    x <- x[-i,]
    
    j <- j + 1
  }
  
  l
}

classification <- function(x,y,nmin,minleaf) {
  cx <- vapply(x,FUN=getClassLabels,numeric(20))
  cx <- (combineParts(cx))[[1]]
  x <- lapply(x,FUN=removeClassLabels)
  x <- combineParts(x)
  cy <- y[,"AHD"]
  #y[,"AHD"] <- NULL
  
  tree <- tree.grow(x,cx,nmin,minleaf)
  tree <- tree.simplify(tree)
  classLabels <- tree.classify(tree,y)
  
  list(cy,classLabels)
}

getClassLabels <- function(x) {
  x[,"AHD"]
}

removeClassLabels <- function(x) {
  x[,"AHD"] <- NULL
  x
}

combineParts <- function(parts) {
  Reduce(function(x,y) {
    rbind(x,y)
  },parts,data.frame())
  
}
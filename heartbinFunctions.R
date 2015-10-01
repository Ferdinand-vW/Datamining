testFunc <- function(x) {
  index <- trainingIndex()
  data <- trainingdata(x,index)
  sample <- testsample(x,index)
  
  dataparts <- splitTrainingData(data)
  
  res <- crossValidation(dataparts)
  
  table(res[1],res[2])
}

crossValidation <- function(parts) {
  i <- 1
  size <- length(parts)
  
  results <- vector("list",2)
  
  while(i <= size) {
    single <- parts[[i]]
    others <- parts[-i]
    
    result <- classify(others,single)
    results[1] <- c(results[1],result[1])
    results[2] <- c(results[2],result[2])
    
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

classify <- function(x,y) {
  cx <- vapply(x,FUN=getClassLabels,numeric(20))
  cx <- (combineParts(cx))[[1]]
  x <- sapply(x,FUN=removeClassLabels)
  x <- combineParts(x)
  cy <- y[,"AHD"]
  y[,"AHD"] <- NULL
  
  tree <- tree.grow(x,cx,nmin = 5,minleaf=5)
  classLabels <- tree.classify(tree,y)
  
  trueOnes <- sum(cy)
  trueZeros <- length(cy) - trueOnes
  predOnes <- sum(classLabels)
  predZeros <- length(classLabels) - predOnes
  list(cy,classLabels)
}

getClassLabels <- function(x) {
  x[,"AHD"]
}

removeClassLabels <- function(x) {
  x[,"AHD"] <- NULL
}

combineParts <- function(parts) {
  Reduce(function(x,y) {
    rbind(x,y)
  },parts,data.frame())
  
}
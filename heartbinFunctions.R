trainingIndex <- function() {
  index <- sample(297,200)
}

partIndex <- function(size) {
  index <- sample(size,20)
}

trainingdata <- function(x,index) {
  x[index,]
}

trainingsample <- function(x,index) {
  x[-index,]
}

splitTrainingData <- function(x) {
  l <- vector("list",10)
  while(length(x[,1]) > 0) {
    i <- partIndex(length(x[,1]))
    part <- x[i,]
    l <- list(l,part)
    x <- x[-i,]
  }
  
  l
}

func <- function(x,y) {
  part1 <- x
  restparts <- x
  
  predY = tree.classify(tree.grow(restpart,nmin = 1,minleaf=1))
  compare(predY,y)
}
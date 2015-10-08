source("Tree.R")
source("Tree_grow.R")
source("Tree_simplify.R")
source("Tree_Classify.R")

#x = dataframe
#returns a tuple with the testsample and trainingdata
#This function simply creates a random testsample and then
#randomly divided the rest of the data into 10 parts
prepareData <- function(x) {
  index <- trainingIndex()
  data <- trainingdata(x,index)
  sample <- testsample(x,index)
  
  dataparts <- splitTrainingData(data)
  
  list(sample,dataparts)
}

#Get a vector with a length of 200 containing randomly chosen
#integers from 1 to 297
trainingIndex <- function() {
  index <- sample(297,200)
}

#size = number of rows in a dataframe
#returns a vector with a length of 20 containing randomly chosen
#integers from 1 to size
partIndex <- function(size) {
  index <- sample(size,20)
}

#x = dataframe
#index = vector of integers
#Returns a dataframe which rows' index correspond
#to the vector index
trainingdata <- function(x,index) {
  x[index,]
}

#Index value is the exact same as above, but this time
#we want to return all the rows that do not correspond
#to the vector index
testsample <- function(x,index) {
  x[-index,]
}

#x = dataframe
#returns a list of dataframes
splitTrainingData <- function(x) {
  #We want to divide the dataframe in 10 parts
  l <- vector("list",10)
  j <- 1
  #As long as there are rows in the dataframe
  while(length(x[,1]) > 0) {
    #randomly take 20 rows
    i <- partIndex(length(x[,1]))
    part <- x[i,]
    
    #and throw these into the list
    l[[j]] <- part
    
    #furthermore remove the taken rows from the dataframe
    x <- x[-i,]
    
    j <- j + 1
  }
  
  l
}

#sample = dataframe (testSample)
#nmin,minleaf = integer
#returns a tuple with the error rate and the resulting tree of the testSample
evaluateSample <- function(sample,nmin,minleaf) {
  y <- sample[,"AHD"]
  x <- sample
  x[,"AHD"] <- NULL
  #We grow the tree and predict the classlabels given the sample
  tree <- tree.grow(x,y,nmin,minleaf)
  tree <- tree.simplify(tree)
  cy <- tree.classify(tree,sample)
  
  #We calculate the classification error by comparing the classlabels from the sample
  #and the predicted classlabels
  error <- classificationError(y,cy)
  
  list(error,tree)
}

#dataparts = list of dataparts with a length of 10
#nmin,minleaf = integer
#returns the summed classification error of the cross validation
#using the dataparts and a given nmin,minleaf
evaluateModel <- function(dataparts,nmin,minleaf) {
  
  res <- crossValidation(dataparts,nmin,minleaf)
  true <- res[[1]]
  pred <- res[[2]]
  classificationError(true,pred)
}

#true = classlabels from a given dataframe
#pred = predicted classlabels calculated by growing and tree
#and then using data to classify against it
#Returns an integer
classificationError <- function(true,pred) {
  #We determine which classlabels were correctly predicted
  corrPred <- true[true==pred]
  #Then we determine how many were fasly predicted
  numE <- length(pred) - length(corrPred)
  #The classification error is then the number of errors divided by
  #the total number of classlabels
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


#parts = list of dataparts with length 10
#nmin,minleaf = integer
crossValidation <- function(parts,nmin,minleaf) {
  #initialize iterator
  i <- 1
  #determine size of dataparts
  size <- length(parts)
  
  results <- vector("list",2)
  
  #We loop over the list of dataparts
  while(i <= size) {
    #First we take a part that wasn't chosen before
    single <- parts[[i]]
    #Then we get all the other parts
    others <- parts[-i]
    
    #The other parts are than used to classify the single part
    result <- classification(others,single,nmin,minleaf)
    #
    results[[1]] <- c(results[[1]],result[[1]])
    results[[2]] <- c(results[[2]],result[[2]])
    
    i <- i + 1
  }
  
  results
}

#x = list of dataframes
#y = single dataframe
#nmin,minleaf = integers
#returns a 2-tuple with the given classlabels and the predicted classlabels
classification <- function(x,y,nmin,minleaf) {
  #First we want to get all classlabels of each dataframe
  #and then combine them into a vector
  cx <- vapply(x,FUN=getClassLabels,numeric(20))
  cx <- (combineParts(cx))[[1]]
  
  #Then we want to remove the classlabels from the dataframes
  #and combine the dataframes into a single dataframe
  x <- lapply(x,FUN=removeClassLabels)
  x <- combineParts(x)
  
  #Furthermore we want the 'true' classlabels from the single dataframe
  cy <- y[,"AHD"]
  
  #We can now grow the tree using the combined dataframes and their classlabels
  tree <- tree.grow(x,cx,nmin,minleaf)
  tree <- tree.simplify(tree)
  #Then we classify the single dataframe
  classLabels <- tree.classify(tree,y)
  
  #list(true,pred)
  list(cy,classLabels)
}

#x = dataframe
#returns column AHD of x
getClassLabels <- function(x) {
  x[,"AHD"]
}

#x = dataframe
#removes column AHD of x
removeClassLabels <- function(x) {
  x[,"AHD"] <- NULL
  x
}

#parts = list of dataframes
#Uses fold and rbind to append each dataframe to each other
#resulting into a single big dataframe
combineParts <- function(parts) {
  Reduce(function(x,y) {
    rbind(x,y)
  },parts,data.frame())
  
}
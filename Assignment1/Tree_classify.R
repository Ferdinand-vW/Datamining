source("Tree.R")

#tr = tree structure
#dt = dataframe including classlabels
#returns the predicted classlabels
tree.classify <- function(tr,dt) {
  #We classify each row of the dataframe
  #Each classified row returns a 1 or 0
  apply(dt,1,function(x) classify(x,tr))
}

#row = a row from a dataframe
#n = node
#returns a 1 or 0
classify <- function(row,n) {
  #If we're not at the bottom of the tree we
  #recursively go through the tree using the 
  #split attribute and value, which we compare to
  #the data in the row
  if(!isLeaf(n)) {
    attr <- n@splitAttr
    val <- n@splitVal

   if(row[attr] <= val) {
     classify(row,n@lChild)
   }
   else {
     classify(row,n@rChild)
   }
 }#When we are at the bottom we can determine the class
 else {
  detClass(n)
 }
}

#n = node
#returns 1 or 0
detClass <- function(n) {
  Lbl <- n@classLabels
  #Number of ones/zeros
  num_1 <- sum(Lbl)
  num_0 <- length(Lbl) - num_1
  #If there are more zeros than ones then we
  #assign 0 as the classlabel,otherwise 1
  if(num_0 > num_1) {
    0
  }
  else if(num_1 > num_0){
    1
  }#if however there are an equal number of 1s and 0s
  #then we randomly choose one out of the two
  else {
    sample(c(0,1),1)
  }
}




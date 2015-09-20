bestsplit <- function(x,y,minleaf) {
  #First we sort the values of the attribute
  sorted <- sort(unique(x))
  #Then we determine the splitpoints, which are the average values of all two consecutive values
  splitpoints <- (sorted[1:length(sorted) - 1] + sorted[2:length(sorted)]) / 2
  
  #Now determine the split and return it together with its quality
  split <- foldSplits(x,y,splitpoints,minleaf)
  c(split,computesplit(x,y,split,minleaf))
}

foldSplits <- function(x,y,splits,minleaf) {
  #We use a fold function to determine the best split
  #The fold function has a neutral value. It then computes the
  #split quality of the neutral value and the first element of the given list
  #It compares these and returns the best split, which is the compared with the second
  #value in the list and so on...
  Reduce(function(a,b) {
    #first compute the split for a given split value a and b
    asplit <- computesplit(x,y,a,minleaf)
    bsplit <- computesplit(x,y,b,minleaf)
    
    #As it is possible that a split is not possible given a split value
    #the computesplit function can return NA, which means no split was possible
    #In that case we need to check whether a or b, both or neither are NA
    #and then according return the correct value
    if(!is.na(asplit) && !is.na(bsplit)) {
      if(asplit > bsplit) {
        a
      }
      else {
        b
      }
    }
    else if(!is.na(asplit)){
      a
    }
    else if(!is.na(bsplit)){
      b
    }
    else {
      NA
    }
  }, splits, NA)
}

computesplit <- function(x, y, val,minleaf) {
  
  #split the class labels using val and the given attribute values
  if(!is.na(val)) {
    lchild <- y [x <= val]
    rchild <- y [x > val]
    
    #If no split can be done return NA
    if(!canbeSplit(lchild, rchild,minleaf)) {
      NA
    }
    else {
      #calculate how many samples are now in the left/right child
      #of the total number of samples
      ldistr <- length(lchild) / length(x)
      rdistr <- length(rchild) / length(x)
      
      #calculates the quality of the split
      valOfChild(y) - (ldistr * valOfChild(lchild) + (rdistr * valOfChild(rchild)))
    }
  }
  else {
    NA
  }
}

#A split is only possible if the child nodes have at least minleaf samples
canbeSplit <- function(l, r,minleaf) {
  length(l) >= minleaf && length(r) >= minleaf
}

#gini index function
valOfChild <- function(x) {
  pGood <- sum(x) / length(x)
  pBad <- length(x[x==0]) / length(x)
  pGood * pBad
}
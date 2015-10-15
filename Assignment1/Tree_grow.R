source("Tree.R")
source("Splitting.R")

#x = dataframe without classlabels
#y = classlabels
#nmin,minleaf are integers
#The result is a tree --see Tree.R for the structure of the tree
tree.grow <- function(x,y,nmin,minleaf) {
  #We start by creating a root node and adding all class labels to it
  root <- newNode(y)#Then we use the root to grow the rest of the tree
  tree.grow1(x,root,nmin,minleaf)
}

#x = dataframe without classlabels
#node = a node or leaf
#The result is also a tree
tree.grow1 <- function(x,node,nmin,minleaf) {
  y <- node@classLabels
  
  #We can only split the current node if it contains enough class labels
  if(length(y) >= nmin) {
    #We look for a split using the given data
    split <- findSplit(x,y,minleaf)
    
    #If the result of the split is NA
    #then no split was possible
    if(!is.na(split[[1]])) {
      #Do the split, which returns a new node, containing two child nodes
      n <- doSplit(x,y,split)
      
      #We now have to split rows of the given dataframe
      #using the given split value and split attribute
      lx <- x[x[split[[2]]] <= split[[1]], ]
      rx <- x[x[split[[2]]] >  split[[1]], ]
      
      #use the tree grow function on both the left and right child
      #and update the current node
      lChild <- tree.grow1(lx,n@lChild,nmin,minleaf)
      n@lChild = lChild
      
      rChild <- tree.grow1(rx,n@rChild,nmin,minleaf)
      n@rChild = rChild
      
      return(n)
    }
  }
  node
}

#x = dataframe
#y = classlabels
#This function calculates the best split over all attributes
findSplit <- function(x,y,minleaf) {
  #Foreach column in x we use the findsplitAttribute function with
  #the given y and minleaf
  #The result is a list of 2-tuples with a splitattribute and its splitvalue
  splits <- lapply(x,FUN=findSplitAttribute,y=y,minleaf=minleaf)
  
  #Initialize 'best' values of split,gini and attribute
  bSplit <- NA
  bgini <- NA
  battr <- c("")
  
  #determine of the above calculated splits, which is best
  for(col in names(splits)) {
    #First unpack the data
    attr <- splits[[col]]
    split <- attr[1]
    gini <- attr[2]
    
    #ensure that a split was actually possible
    if(!is.na(split)) {
      #If the new gini is better than the previous one
      #update the best values
      if(is.na(bgini) || gini > bgini) {
        bgini <- gini
        bSplit <- split
        battr <- col
      }
    }
  }
  
  #return the splitvalue and the attribute that was used for splitting
  list(bSplit,battr)
}

#find a split for a specific attribute
findSplitAttribute <- function(col,y,minleaf) {
  bestsplit(col,y,minleaf)
}

#use a split value and attribute to split
#the class labels of the node and to create two
#new nodes, which are the child nodes
#x = dataframe
#y = classlabel
#split = 2-tuple of the splitvalue and its attribute
#returns a node with two children
doSplit <- function(x,y,split) {
  splitVal <- split[[1]]
  splitAttr <- split[[2]]
  
  #Determine which classlabels go into the left/right node
  lChild <- y[x[splitAttr] <= splitVal]
  rChild <- y[x[splitAttr] > splitVal]
  
  #Create a new node using the old classlabels
  #and add a right and left child node
  n <- newNode(y)
  n@lChild = newNode(lChild)
  n@rChild = newNode(rChild)
  n@splitAttr = splitAttr
  n@splitVal = splitVal
  
  n
}
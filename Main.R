source("Tree.R")
source("functions.R")

tree.grow <- function(x,y,nmin,minleaf) {
  #We start by creating a root node and adding all class labels to it
  root <- newNode(y)
  tree.grow1(x,root,nmin,minleaf)
}

tree.grow1 <- function(x,node,nmin,minleaf) {
  y <- node@classLabels
  
  #We can only split the current node if it contains enough class labels
  if(length(y) >= nmin) {
    split <- findSplit(x,y,minleaf)
    
    #Determine whether a split was possible
    if(!is.na(split[[1]])) {
      #Do the split, which returns a new node, containing two child nodes
      n <- doSplit(x,y,split)
      
      #Remove all unnecessary rows
      lx <- x[x[split[[3]]] <= split[[1]], ]
      rx <- x[x[split[[3]]] >  split[[1]], ]
      
      #There must be at least one more column to split on
      if(length(colnames(lx)) > 1) {
        #we can now safely remove the previously split on column
        lx[split[[3]]] <- NULL 
        #recursively split into the left child node
        lChild <- tree.grow1(lx, n@lChild,nmin,minleaf)
        n@lChild = lChild
      }
      
      #Do the same as above for the right childnode
      if(length(colnames(rx)) > 1) {
        rx[split[[3]]] <- NULL
        rChild <- tree.grow1(rx, n@rChild,nmin,minleaf)
        n@rChild = rChild
      }
      
      return(n)
    }
    else {
    }
  }
  else {
    print("class labels have no length")
  }
  
  node
}

findSplit <- function(x,y,minleaf) {
  #returns the best split and its quality for each attribute
  splits <- lapply(x,FUN=findSplitAttribute,y=y,minleaf=minleaf)
  
  bSplit <- NA
  bgini <- NA
  battr <- c("")
  
  #determine of the above calculated splits, which is best
  for(col in names(splits)) {
    attr <- splits[[col]]
    split <- attr[1]
    gini <- attr[2]
    
    #ensure that a split was actually possible
    if(!is.na(split)) {
    
      if(is.na(bgini) || gini > bgini) {
        bgini <- gini
        bSplit <- split
        battr <- col
      }
    }
  }
  
  #return the splitvalue, its quality and the attribute that was used for splitting
  list(bSplit,bgini,battr)
}

#find a split for a specific attribute
findSplitAttribute <- function(col,y,minleaf) {
  bestsplit(col,y,minleaf)
}

#use a split value and attribute to split
#the class labels of the node and to create two
#new nodes, which are the child nodes
doSplit <- function(x,y,split) {
  splitVal <- split[[1]]
  gini <- split[[2]]
  splitAttr <- split[[3]]

  lChild <- y[x[splitAttr] <= splitVal]
  rChild <- y[x[splitAttr] > splitVal]
  
  n <- newNode(y)
  n@lChild = newNode(lChild)
  n@rChild = newNode(rChild)
  n@splitAttr = splitAttr
  n@splitVal = splitVal
  
  n
}


# 
# #If a node and both children are leafs and have the same majority class
# #then remove the children and return the current node back as a leaf node
# #If a node but the children are not leaf nodes then recursively go into both the left
# #and right child
# #If the current node is a leaf node then simply return it
# tree.simplify <- function(tree) {
#   if(isNode(tree)) {
#     l <- leftChild(tree)
#     r <- rightChild(tree)
#     
#     if(isLeaf(l) && isLeaf(r) && class(l) == class(r)) {
#       leaf(vaue(tree))
#     }
#     else {
#       l <- tree.simplify(l)
#       r <- tree.simplify(r)
#       node(l, value(tree), r)
#     }
#   }
#   else {
#     tree
#   }
# }
# 
# tree.classify <- function(x,tr) {
#   y <- c(0)
#   
#   foreach(sample s : x) {
#       y <- c(y, tree.classify1(s,tr))
#   }
#   
#   y
# }
# 
# 
# #We check whether the tree is a node
# #If so we get the attribute that the node is split on
# #Furthermore we get the value that was used for splitting
# #Then we take the corresponding value of the training sample s
# #If that value is lower than the splitvalue we recursively go into the leftchild
# #otherwise into the rightchild
# #The base case is the leaf node, which returns the majority class of that leaf
# tree.classify1 <- function(s, tr) {
#   if(isNode(tr)) {
#     attr <- attribute(tr)
#     splitvalue <- split(tr)
#     
#     val <- attributeValue(s, attr)
#     if (val <= splitvalue) {
#       l <- leftChild(tr)
#       tree.classify1(s, l)
#     }
#     else {
#       r <- rightChild(tr)
#       tree.classify1(s, r)
#     }
#   }
#   else {
#     classValue(tr)
#   }
# }
  
  




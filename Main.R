tree.grow <- function(x,y,nmin,minleaf) {
  n <- node(y)
  tree.grow1(x,n,nmin,minleaf)
}

#First we find a value and attribute to split on
#Then actually do the split, which should return a node with two leafs
#Then we recursively do the above steps for both the leftchild and the rightchild
tree.grow1 <- function(x,n,nmin,minleaf) {
  
  split <- findSplit(x,n,nmin,minleaf)
  n <- doSplit(split,n)
  
  lChild <- tree.grow1(x, leftChild(n),nmin,minleaf)
  rChild <- tree.grow1(x, rightChild(n),nmin,minleaf)
  
  node(lChild, value(n), rChild)
}

findSplit <- function(x, y, nmin, minleaf) {
  #for(Attribute a in x) {
   # findSplitAttribute(a, y, nmin, minleaf)
  #}
}

findSplitAttribute <- function(attr, y, nmin, minleaf) {
  
}

#If a node and both children are leafs and have the same majority class
#then remove the children and return the current node back as a leaf node
#If a node but the children are not leaf nodes then recursively go into both the left
#and right child
#If the current node is a leaf node then simply return it
tree.simplify <- function(tree) {
  if(isNode(tree)) {
    l <- leftChild(tree)
    r <- rightChild(tree)
    
    if(isLeaf(l) && isLeaf(r) && class(l) == class(r)) {
      leaf(vaue(tree))
    }
    else {
      l <- tree.simplify(l)
      r <- tree.simplify(r)
      node(l, value(tree), r)
    }
  }
  else {
    tree
  }
}

tree.classify <- function(x,tr) {
  y <- c(0)
  
  foreach(sample s : x) {
      y <- c(y, tree.classify1(s,tr))
  }
  
  y
}


#We check whether the tree is a node
#If so we get the attribute that the node is split on
#Furthermore we get the value that was used for splitting
#Then we take the corresponding value of the training sample s
#If that value is lower than the splitvalue we recursively go into the leftchild
#otherwise into the rightchild
#The base case is the leaf node, which returns the majority class of that leaf
tree.classify1 <- function(s, tr) {
  if(isNode(tr)) {
    attr <- attribute(tr)
    splitvalue <- split(tr)
    
    val <- attributeValue(s, attr)
    if (val <= splitvalue) {
      l <- leftChild(tr)
      tree.classify1(s, l)
    }
    else {
      r <- rightChild(tr)
      tree.classify1(s, r)
    }
  }
  else {
    classValue(tr)
  }
}
  
  




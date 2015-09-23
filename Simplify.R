source("Tree.R")

# #If a node and both children are leafs and have the same majority class
# #then remove the children and return the current node back as a leaf node
# #If a node but the children are not leaf nodes then recursively go into both the left
# #and right child
# #If the current node is a leaf node then simply return it


tree.simplify <- function(tree) {
   
  if(isNode(tree)) {
     l <- tree@lChild
     r <- tree@rChild
     
     
     #compare the leafs and the class labels 
     if(isLeaf(l) && isLeaf(r) ){
       l_maj <- majority(l)
       r_maj <- majority(r)

      if (l_maj == r_maj || l_maj == 2 || r_maj == 2) {
      
           tree@lChild <- new("Leaf")
           tree@rChild <- new("Leaf")
           tree@splitAttr <- character(0)
           tree@splitVal <- numeric(0)
      }
       tree
     } 
     #recursive with left and right child
     else {
       l_sim <- tree.simplify(l)
       r_sim <- tree.simplify(r)
  
       tree@lChild <- l_sim
       tree@rChild <- r_sim
       
       if(!identical(l_sim, l) || !identical(r_sim, r)) {
         tree.simplify(tree)
       }
       else {
         tree
       }

     }
   }
   else {
     tree
   }
}

majority <- function(x){
  Lbl <- x@classLabels
  value_1 <- sum(Lbl)
  value_0 <- length(Lbl) - value_1
  if (value_0 > value_1) {
    0
  }
  else if (value_0 < value_1){
    1
  }
  else {
    2
    
  }
  
}

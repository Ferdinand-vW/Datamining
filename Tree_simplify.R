source("Tree.R")

#tree = tree/node
tree.simplify <- function(tree) {
  #If the current tree is not a node
  #then we are done, otherwise...
  if(isNode(tree)) {
     l <- tree@lChild
     r <- tree@rChild
     
     
     #If both child nodes are leafs, then we determine their majority
     if(isLeaf(l) && isLeaf(r) ){
       l_maj <- majority(l)
       r_maj <- majority(r)
      
      #If they have an equal majority, or one of the two nodes has no majority
      #then we can remove both child nodes
      if (l_maj == r_maj || l_maj == 2 || r_maj == 2) {
      
           tree@lChild <- new("Leaf")
           tree@rChild <- new("Leaf")
           tree@splitAttr <- character(0)
           tree@splitVal <- numeric(0)
      }
       tree
     } 
     #If both child nodes are no leaves, then we recursively go through the tree
     else {
       l_sim <- tree.simplify(l)
       r_sim <- tree.simplify(r)
       
       #We assign the newly simplified child nodes to the current node
       tree@lChild <- l_sim
       tree@rChild <- r_sim
       
       #If one of the childnodes is different after simplification from before we
       #simplified the node, then we redo the simplification on the current node
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

#n = node
#returns a 0,1 or 2
#determines the majority in the classlabels of a given node
majority <- function(n){
  Lbl <- n@classLabels
  num_1 <- sum(Lbl)
  num_0 <- length(Lbl) - num_1
  #If 0 has a majority then we simply return 0
  #and 1 if 1 has the majority
  if (num_0 > num_1) {
    0
  }
  else if(num_1 > num_0){
    1
  }#However if there is no majority, then we return a two
  else {
    2
  }
  
}

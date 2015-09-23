source("Tree.R")
source("simplify.R")

tree.classify <- function(tr,dt) {
  apply(dt,1,function(x) classify(x,tr))
}

classify <- function(sample,tr) {
   if(!isLeaf(tr)) {
     attr <- tr@splitAttr
     val <- tr@splitVal

     if(sample[attr] <= val) {
       classify(sample,tr@lChild)
     }
     else {
       classify(sample,tr@rChild)
     }
   }
  else {
    majority(tr)
  }
}




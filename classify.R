source("Tree.R")

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
    defClass(tr)
  }
}
  
defClass <- function(tr) {
  Lbl <- tr@classLabels
  value_1 <- sum(Lbl)
  value_0 <- length(Lbl) - value_1
  if(value_0 > value_1) {
    0
  }
  else if(value_1 > value_0){
    1
  }
  else {
    sample(c(0,1),1)
  }
}




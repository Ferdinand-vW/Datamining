root <- list(
        list(list(
            list(),4,list()
                        ),1,list()),2,(list(list(),3,list())))



node <- function(left=leaf(),val,right=leaf()) {
  list(left, val, right)
}

leaf <- function() {
  list()
}

leftChild <- function(val) {
  if (isNode(val)) {
    val[[1]]
  }
  else {
    message("This is a leaf node and has no children")
    stop()
  }
}

rightChild <- function(val) {
  if(isNode(val)) {
    val[[3]]
  }
  else {
    message("This is a leaf node and has no children")
    stop()
  }
}

value <- function(val) {
  if(isNode(val)) {
    val[[2]]
  }
  else {
    message("This is a leaf node and has no value")
    stop()
  }
}

isNode <- function(val) {
  length(val) == 3
}

isLeaf <- function(val) {
  length(val) == 1
}

addOne <- function(x) {
  x + 1
}

foldApply <- function(FUN, n) {
  if(isNode(n)) {
    l <- foldApply(FUN,leftChild(n))
    val <- FUN(value(n))
    r <- foldApply(FUN,rightChild(n))

    node(l,val,r)
  }
  else {
    n
  }
}
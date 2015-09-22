setClass(
  "Tree"
)

setClass(
  "Node",
  representation(lChild="Tree",
                 classLabels="numeric",
                 splitAttr="character",splitVal="numeric",
                 rChild="Tree"),
  contains = "Tree"
)

setClass(
  "Leaf",
  contains = "Tree"
)
setGeneric("isLeaf",function(x) attributes(x))
setMethod("isLeaf","Tree",
          function(x) {
            if(isNode(x)) {
              class(x@lChild) == "Leaf" && class(x@rChild) == "Leaf"
            }
            else {
              FALSE
            }
            
          })

setGeneric("isNode",function(x) attributes(x))
setMethod("isNode", "Tree",
          function(x) {
            class(x) == "Node"
          })

newNode <- function(x) {
  new("Node", lChild= new("Leaf"), classLabels=x, rChild=new("Leaf"))
}



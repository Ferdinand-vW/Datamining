library("plotrix")

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

setGeneric("heightOf",function(x) attributes(x))
setMethod("heightOf","Tree",
          function(x) {
            if(isNode(x)) {
              1 + max(heightOf(x@lChild),heightOf(x@rChild))
            }
            else {
              0
            }
          })

setGeneric("sizeOf",function(x) attributes(x))
setMethod("sizeOf","Tree",
          function(x) {
            if(isNode(x)) {
              1 + sizeOf(x@lChild) + sizeOf(x@rChild)
            }
            else {
              0
            }
          })

setGeneric("widthOf",function(x) attributes(x))
  setMethod("widthOf","Tree",
            function(x) {
              h <- heightOf(x)
              2^(h-1)
            })

setMethod("plot","Tree",
          function(x) {
            
            par(mar = c(4,4,4,4)) 
            w <- widthOf(x)
            h <- heightOf(x)
            plot.new()
            plot.window(c(0,w * 50),c(0,h * 75),asp=1)
            plotNode(x,w * 25,(h*75) - 25,w*25)
          })

setGeneric("plotNode",function(n,x,y,scale) attributes(n,x,y,scale))
setMethod("plotNode","Tree",
          function(n,x,y,scale) {
            
            numOnes <- sum(n@classLabels)
            numZeros <- length(n@classLabels) - numOnes
            
            #Draw the node
            draw.circle(x,y,nv=100,radius=25)
            
            #Draw text inside the nodey
            text(x - 12.5,y,numOnes)
            text(x + 12.5,y,numZeros)
            lines(c(x,x),c(y+25,y-25))
            
            
            if(!isLeaf(n) && isNode(n)) {
              newX <- scale / 2
              newY <- y - 50
              
              #Draw the edges
              lines(c(x,x-newX),c(y-25,newY))
              lines(c(x,x+newX),c(y-25,newY))
              
              plotNode(n@lChild,x - newX,newY - 25,newX)
              plotNode(n@rChild,x + newX,newY - 25,newX)
            }
          })



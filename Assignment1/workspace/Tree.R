# library("plotrix")

#Abstract Tree class
setClass(
  "Tree"
)

#Class node that represents an internal node within our tree
setClass(
  "Node",
  representation(lChild="Tree",
                 classLabels="numeric",
                 splitAttr="character",splitVal="numeric",
                 rChild="Tree"),
  contains = "Tree"
)

#Class leaf, doesn't contain an actual value and acts merely as a placeholder
#for the children of the nodes at the bottom of the tree
setClass(
  "Leaf",
  contains = "Tree"
)

#The name might be misleading, but the method determines whether a given node
#is at the bottom of the tree. This means it must still be an internal node!
setGeneric("isLeaf",function(x) attributes(x))
setMethod("isLeaf","Tree",
          function(x) {
            if(isNode(x)) { #If an internalnode
              class(x@lChild) == "Leaf" && class(x@rChild) == "Leaf" 
            } #if both children are leaves, then it is a 'leaf' node
            else {
              FALSE
            }
            
          })

setGeneric("isNode",function(x) attributes(x))
setMethod("isNode", "Tree",
          function(x) {
            class(x) == "Node"
          })

#Simple function, which takes a vector of integers and creates a new node object
#with the given vector for its classlabels
newNode <- function(x) {
  new("Node", lChild= new("Leaf"), classLabels=x, rChild=new("Leaf"))
}


#-------------------------------------------------------------------------------------
#The code below this part is purely to help draw the tree and for no other purpose that is
#related to the analysis of heartbin

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


#To use this code uncomment it, uncomment the library at the top of this page
#and install the given library. The code below wasn't a necessary part of the assignment
#but we used it to graph our tree. Understand that it works best for our specific tree,
#if you try to graph with a different tree, text will most likely be misaligned

# setMethod("plot","Tree",
#           function(x) {
#             
#             par(mar = c(4,4,4,4)) 
#             w <- widthOf(x)
#             h <- heightOf(x)
#             plot.new()
#             plot.window(c(0,w * 100),c(0,h * 100),asp=1)
#             plotNode(x,w * 50,(h*100) - 25,w*50,25)
#             
#             rect(w * 80 - 25,(h*100) - 50,w*80 + 25,(h*100))
#             rect(w*80+25,(h*100) - 50,w*80 + 75,h*100)
#             text(w*80,(h*100) - 25,"Bad")
#             text(w*80 + 50,h*100 - 25,"Good")
#           })
# 
# setGeneric("plotNode",function(n,x,y,scale,radius) attributes(n,x,y,scale,radius))
# setMethod("plotNode","Tree",
#           function(n,x,y,scale,radius) {
#             
#             numOnes <- sum(n@classLabels)
#             numZeros <- length(n@classLabels) - numOnes
#             
#             if(!isLeaf(n) && isNode(n)) {
#               newX <- scale / 2
#               newY <- y - (radius * 3)
#               
#               #Draw the edges
#               lines(c(x,x-newX),c(y-radius,newY))
#               lines(c(x,x+newX),c(y-radius,newY))
#               
#               #Draw split information
#               ltext <- paste("<=",n@splitVal)
#               rtext <- paste(">",n@splitVal)
#               attrtext <- paste(n@splitAttr)
#               text(x - (newX / 2) - (length(ltext) * 20),y - (radius * 1.5),ltext)
#               text(x + (newX / 2) + (length(rtext) * 20),y - (radius * 1.5),rtext)
#               text(x,y - (2 *radius),attrtext)
#               
#               #Draw the child nodes
#               plotNode(n@lChild,x - newX,newY - radius,newX,radius)
#               plotNode(n@rChild,x + newX,newY - radius,newX,radius)
#               
#               #Draw the node
#               rectFill(x - radius,y - radius,x,y + radius,pch = NULL,bg=5)
#               rectFill(x,y - radius, x + radius, y + radius,pch = NULL,bg=5)
#             }
#             else{
#               color <- c(0)
#               if(numZeros > numOnes) {
#                 color <- 2 #red
#               }
#               else {
#                 color <- 3 #green
#               }
#               #Draw the node with a given color
#               rectFill(x - radius,y - radius,x,y + radius,pch = NULL,bg=color)
#               rectFill(x,y - radius, x + radius, y + radius,pch = NULL,bg=color)
#             }
#             
#             zeroLength <- radius / length(paste(numZeros))
#             oneLength <- radius / length(paste(numOnes))
#             #Draw text inside the node
#             text(x - zeroLength / 2,y,numZeros)
#             text(x + oneLength / 2,y,numOnes)
#           })
# 
# 

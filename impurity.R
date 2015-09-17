#Impurity with Gini-index
impurity <- function (y){
  ((length(y)-sum(y)) / length(y)) * (1- ((length(y)-sum(y)) / length(y)))
  
}

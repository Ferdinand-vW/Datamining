#Bestsplit function

bestsplit <- function (x,y){
#Sort the x value
 income <- c(0)
 income <- sort(unique(x))
 
#get the split value of x
 newincome <- c(0)
 for (i in 1:(length(income)-1)){
 newincome[i] <- (income[i] + income[i+1]) / 2
 }
 
#parent impurity
 parent_imp <- impurity(y)
 
#create child
 imp_red <- c(0)
 for (i in 1:length(newincome)) {
 value <- newincome[i]

 #left child
 l_percentage <- (length(y[x <= value]) / length(y))
 l_impurity <- impurity(y [x <= value])
 l_quality <- l_impurity * l_percentage
 
#right child
 r_percentage <- (length(y[x > value]) / length(y))
 r_impurity <- impurity(y [x > value])
 r_quality <- r_impurity * r_percentage
 
 imp_red[i] <- (parent_imp - l_quality - r_quality)
 
 }
#Show the value of newincome that has the max reduction impurity
 newincome[which.max(imp_red)]
 
}

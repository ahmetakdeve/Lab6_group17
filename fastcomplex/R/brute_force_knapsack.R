#' Brute force search
#' 
#' Solve the knapsack problem by using brute force search algorithm. This algorithm solves the knapsack problem by going through all possible combinations and return the highest value.
#' 
#' @param x data frame containing variables "v", values, and "w" which is weights of the items.
#' @param W Knapsack size 
#' 
#' @return A list containing the value and the picked elements
#' 
#' @export

brute_force_knapsack<-function(x,W){
  stopifnot(class(x)=="data.frame" & dim(x)[2]==2 & names(x)[1]=="w" & names(x)[2]=="v")
  #x<-x[x$w<W,]
  mylist<-list() #Create empty vector
  weights<-vector() #Empty vector. Will be filled with summed weights of different combinations
  values<-vector()  #Empty vector. Will be filled with summed values of different combinations
  for(i in 1:2^nrow(x)){
    mylist[[i]] <- as.numeric(intToBits(i)[1:nrow(x)]) #Binary vectors saved in the list
    pickones<-x[mylist[[i]] == 1,] #Pick out values from specific rows of our data frame which is used in a spec combo
    weights[i]<-sum(pickones[,1])  #Sum up the weights and store in the vector
    values[i]<-sum(pickones[,2])   #Sum up the values and store in the vector
}
passedrows <- which(weights<W) #We want only the rows there the summed weights of the combo are less than W
passedW <- weights[passedrows] #The actual weights less than W
passedV <- values[passedrows]  #Values of the passed rows are picked out from the "values" vector
highestvalue <- which.max(passedV) #The row which contains the highest value

elements <- (1:nrow(x))[mylist[[passedrows[highestvalue]]]==1] #Get the row from the list which contains the best combo
elements <- elements[order(elements, decreasing = FALSE)] #Set elements from smallest to biggest
return(list(value=values[passedrows[highestvalue]], elements=elements)) #Create a list which looks like in the instructions
}


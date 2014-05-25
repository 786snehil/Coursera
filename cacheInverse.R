#function to compute a special matrix 
#which can be used to cache the inverse of matrix
#Assuming x to be a invertible matrix

makeMatrix <- function(x) {
  inv <- NULL  # cache variable initialized to NULL
  
  #function to find the inverse for the first time
  set <- function(y) {
  x <<- solve(y)
  inv <<- NULL  
}
  
  # function returning the target matrix - whose inverse is to be calculated
  get <- function() x   
  
  #function to set value to cache variable when inverse obtained for same x
  setInverse <- function(x) inv <<- x
  
  #function to get the value of cache variable
  getInverse <- function() inv
  
  #special matrix containing the  4 function ,therefore dimension of matrix 1 x 4 
  matrix( c(set , get ,setInverse , getInverse), nrow = 4, ncol =1 ,dimnames = list(c("set","get","setInverse","getInverse")))
  
 
}

#function that calculates the inverse and tells if cache is used
cacheInverse <- function(x, ...) {
  
  inv <- x[,1]$getInverse()
  
  # if loop to check the value of cache variable 
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x[,1]$get()
  inv <- solve(data, ...)
  x[,1]$setInverse(inv)
  inv
}

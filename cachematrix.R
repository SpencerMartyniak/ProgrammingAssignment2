## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  #mat is our matrix object
  mat <- NULL
  
  #set value of special matrix
  set <- function(y) { 
    x <<- y 
    mat <<- NULL
  }
  
  #get the value of the special matrix
  get <- function() x
  
  #set the value of the inverse
  setinverse <- function(solve) mat <<- solve(mat)
  
  #get the value of the inverse
  getinverse <- function() mat
  
  
  #return special "matrix" object. really just a list with cached inverse
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {  
  ## check if the inverse has been computed
  mat <- x$getinverse()
  if(!is.null(mat)) {
    message("getting cached data")
    return(mat)
  }
  
  #if it hasnt been computed, set a new inverse
  newmat <- x$get()
  mat <- solve(newmat)
  x$setinverse(mat)
  mat
}

## Put comments here that give an overall description of what your
## functions do

## This function creates a 'special matrix' which can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  invMatCache <- NULL
  # Set the value of the matrix
  set <- function(y) {
    x <<- y
    invMatCache <<-NULL
  }
  # Get the value of the matrix
  get <- function() x
  
  # Set the value of the inverse
  setinverse <- function(inverse) invMatCache <<- inverse 
  # Get the value of the inverse
  getinverse <- function() invMatCache
  
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function returns the inverse of a matrix. It does 
## first check if the inverse is available in cache else
## it computes it and stores for future reference
cacheSolve <- function(x, ...) {
  invMat <- x$getinverse()
  
  # Check if inverse is present in cache
  if(!is.null(invMat)){
    message("Getting cached data")
    return(invMat)
  }
  
  # No cache available, compute anew
  data <- x$get() # Get Data
  invMat <- solve(data, ...)  # Compute inverse
  x$setinverse(invMat)  # Store in cache
  invMat   # Return inverse of the matrix
    
        ## Return a matrix that is the inverse of 'x'
}

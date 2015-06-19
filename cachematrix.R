
makeCacheMatrix <- function(x = matrix()) {
  ## x is a square invertible matrix
 
  ## this function returns a list that is used as the input to cacheSolve()
  
  inv = NULL
  set = function(y) {
    # `<<-` is used here to assign a value to x in an environment different from the current environment. 
    x <<- y
    invrs <<- NULL
  }
  get = function() x
  setinvrs = function(inverse) invrs <<- inverse 
  getinvrs = function() invrs
  list(set=set, get=get, setinvrs=setinv, getinvrs=getinv) #this list is returned
}

cacheSolve <- function(x, ...) {
  ## x is the output of makeCacheMatrix()
  ## This function returns inverse of the original matrix
  
  invrs = x$getinvrs()
  
  # if the inverse is available from before
  if (!is.null(invrs)){
    # gets it from the cache and not calculate again
    message("getting cached data")
    return(invrs)
  }
  
  # if the inverse is not available, it is calculated
  mat.data = x$get()
  invrs = solve(mat.data, ...)
  
  # sets the inverse in the cache for future use
  x$setinv(invrs)
  
  return(invrs)
}
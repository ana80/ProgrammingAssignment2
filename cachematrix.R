## Put comments here that give an overall description of what your
## functions do


## allows to initiate and define the matrix creation 

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  # Function get: Return the stored original matrix
  get <- function() x
  # Function setinverse: Put the given matrix into the inverse i
  setinverse <- function(inverse) i <<- inverse
  # Function getinverse: Return the stored inverse matrix
  getinverse <- function() i
  # Return a list with the 4 functions with the same names
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## this function suppose to compute, cash and return the inverse of target matrix in case of its unavaiability
## if the given matrix remains the same, this function will not recalculate but return the result

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     i <- x$getinverse()
     # If the inverse exists, return the cached inverse and ends the function
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
  }
  # If the inverse doen't exist yet: get the original matrix
    data <- x$get()
     # Calculate the inverse of the matrix
    i <- solve(data, ...)
    # Set the cached inverse
    x$setinverse(i)
    i
}

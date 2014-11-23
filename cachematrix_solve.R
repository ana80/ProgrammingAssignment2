## Put comments here that give an overall description of what your
## functions do


## allows to initiate and define the matrix creation 

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## this function suppose to compute, cash and return the inverse of target matrix in case of its unavaiability
## if the given matrix remains the same, this function will not recalculate but return the result

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
  }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}

## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a list to hold the cached data of inverse of a matrix.
## Through this funtion a new value for the inverse of a matric is set and
## returned if asked by the calling function.

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


## Write a short comment describing this function

##This function first checks whether the inverse of the matrix already exists.
## If yes, then it returns the cached data
## If No, then it evaluates the inverse and set the same so that it can be used 
## further, if required.

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

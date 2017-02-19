## These two functions can be used to determine the inverse of a inversible matrix and used cached value if available in order
## to speed up the performance

## creates a list of functions to set a matrix value, get the matrix, set the inverse of the matrix, and get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) i <<- inv 
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## uses the cached value to return the inverse, else invokes makeCacheMatrix function passed as an argument to compute and return 
## the inverse

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

# mat <- matrix(c(1,4,3,2), nrow=2, ncol=2)
# inv <- solve(mat)
# inv <- cacheSolve(makeCacheMatrix(mat))

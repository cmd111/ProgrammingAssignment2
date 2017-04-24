## makeCacheMatrix: This function creates a special "matrix" object that can cache its 
## inverse. cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated 
##(and the matrix has not changed), then the cachesolve retrieves the inverse from the cache.


## The first function, makeCacheMatrix creates a special "vector", 
##which is really a list containing a function to
#1 set the value of the matrix
#2 get the value of the matrix
#3 set the value of the inverse
#4 get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  xinv <- NULL
  set <- function(y) {
    x <<- y
    xinv <<- NULL
  }
  get <- function() x
  setinv <- function(inv) xinv <<- inv
  getinv <- function() xinv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Checks for cached inverse and return the cached value. Calculates inverse of matrix
## if cached value doesn't exist

cacheSolve <- function(x, ...) {
  xinv <- x$getinv()
  if(!is.null(xinv)) {
    message("getting cached data")
    return(xinv)
  }
  
  xinv <- solve(x$get())
  x$setinv(xinv)
  xinv
}

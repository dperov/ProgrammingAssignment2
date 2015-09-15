## Matrix inversion is potentially time-consuming computations. Following fucntions allow
## to cache the the result if a matrix inverse that can be used in subsequent operations.
## Example:
## > x <- matrix(rnorm(4), 2, 2)
## > z <- makeCacheMatrix(x)
## > cacheSolve(z)



## 'makeCacheMatrix' function creates a special "matrix" object that can cache its inverse
## which is really a list containing a function to
##
## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the inverse
## 4.  get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated and the matrix has not changed, then
## cached value of inverse  is returned.
## Otherwise, the inverse of the matrix is caclucalted and stored in the cache via the 
## setinverse function.
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}



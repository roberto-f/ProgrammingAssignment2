## makeCacheMatrix function creates a "special" matrix that cache his values
## and the inverse matrix computed by means of cacheSolve function
##
## Usage:
## z <- makeCacheMatrix(x) where x is a non singular square matrix
## function setCacheMatrix(y) on z permit to change the matrix values. If the
## new matrix is equal to the old one, none is chandeg. If the new matrix id not equal 
## to the old one, the cached inverse matrix is reset
## 
## You can test with:
## > z <- matrix(c(1, 4, 8, 1, 3,7,4,3,11), nrow=3, ncol=3)
## > MyMatrix <- makeCacheMatrix(z)
## > cacheSolve(MyMatrix)
## Compute inverse             <-- first time inverse matrix is computed
## [,1]   [,2]   [,3]
## [1,]  1.5  2.125 -1.125
## [2,] -2.5 -2.625  1.625
## [3,]  0.5  0.125 -0.125
## > cacheSolve(MyMatrix)
## Getting cached inverse matrix <-- second time is retrieved from cache
## [,1]   [,2]   [,3]
## [1,]  1.5  2.125 -1.125
## [2,] -2.5 -2.625  1.625
## [3,]  0.5  0.125 -0.125
makeCacheMatrix <- function(x = matrix()) {
  x_cached <- x
  xi_cached <- NULL
  setCacheMatrix <- function(y = matrix()) {
    meq <- function(y) {
      is.matrix(y) && is.matrix(x_cached) && dim(y) == dim(x_cached) && all(y == x_cached)
    }
    if(!meq(y)) {
      x_cached <<- y
      xi_cached <<- NULL
    }
  }
  getCacheMatrix <-function() x_cached
  getinv <- function() xi_cached
  setinv <- function(inv = matrix()) xi_cached <<- inv 
  list(setCacheMatrix = setCacheMatrix,
       getCacheMatrix = getCacheMatrix,
       getinv = getinv,
       setinv = setinv)
}
## return the inverse of the "special matriz x created by makeCacheMatrix
## if already computed the cached inverse matrix is returned
## Usage:
## y <- cacheSolve(z) where z is a "special matrix" defined by
##      makeCacheMatrix function
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  xs <- x$getinv()
  if(!is.null(xs)) {
    message("Getting cached inverse matrix")
    xs <- x$getinv()
  }
  else {
    message("Compute inverse")
    xs <- solve(x$getCacheMatrix())
    x$setinv(xs)
  }
  return(xs)
}

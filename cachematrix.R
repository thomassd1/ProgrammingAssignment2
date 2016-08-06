## makeCacheMatrix.R
## 08-06-2016 steveT
## This function creates a special "matrix" object that can cache its inverse.
## Usage example:
## myMatrix <- makeCacheMatrix()
## myMatrix$set(matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2))
## myMatrix$get()

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function (y) {
    x <<- y
    inv <<- NULL
  }
  get <-function() x
  setmatr <- function(solve) inv <<- solve
  getmatr <- function() inv
  list (set=set, get=get, setmatr=setmatr, getmatr=getmatr)
}


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
##(and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.
## Usage example:
## cacheSolve(myMatrix)
cacheSolve <- function(x, ...) {
  inv <- x$getmatr()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data,...)
  x$setmatr(inv)
  inv
  ## Return a matrix that is the inverse of 'x'
}
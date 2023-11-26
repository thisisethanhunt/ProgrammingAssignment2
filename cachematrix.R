## Week 3 Assignment

## library(MASS) is used to calculate inverse for non-squared as well as square matrices
library(MASS)

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL                 ## Initiating inverse as NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x         ## Function to get matrix x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() {
    inver <- ginv(x)          ## Using ginv from MASS to calculate the inverse for non-squared matrices
    inver %*% x               ## Function to obtain the inverse of the matrix
  }
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## This is used to get the cache data
cacheSolve <- function(x, ...) {  ## Gets the cache data 
  inv <- x$getinv()
  if (!is.null(inv)) {           ## Checking whether the inverse is NULL
    message("Getting cached data!")
    return(inv)                   ## Returns cached inverse value
  }
  data <- x$get()
  inv <- solve(data, ...)        ## Calculates inverse value
  x$setinv(inv)
  inv                            ## Return a matrix that is the inverse of x
}

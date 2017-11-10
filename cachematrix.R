## Lexical Scoping

## Put comments here that give an overall description of what your
## functions do

##   A matrix with functions to get/set value & get/set inverse
makeCacheMatrix <- function(x = matrix()) {
  ## cached inverse of matrix
  inv <- NULL
  
  ## getter and setter for matrix
  get <- function() x
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  ## getter and setter for inverse
  
  getinv <- function() inv
  setinv <- function(inverse) inv <<- inverse
  
  
  ## return list of functions for matrix
  list(get=get, set=set, getinv=getinv, setinv=setinv)
}

## Calculates the inverse of a matrix. 
## If the inverse has already been calculated before, 
## it returns the cached inverse.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()

  ## if the inverse has already been calculated
  if (!is.null(inv)) {
    message("inverse is cached")
    return(inv)
  }
  
  ## calculate inverse of matrix 
  m <- x$get()
  inv <- solve(m, ...)
  
  ## cache inverse
  x$setinv(inv)
  
  ## return inverse
  return(inv)
}

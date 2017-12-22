## Functions for computing matrix inverses and saving a cache
## to remove the need for recomputation

## makeCacheMatrix returns an object with methods for getting/setting
## a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  matr <- x
  inv <- NULL
  get <- function() matr
  set <- function(newMatr) {
    matr <<- newMatr
    inv <<- NULL
  }
  getinv <- function() inv
  setinv <- function(newInv) {
    inv <<- newInv
  }
  list(set = set, setinv = setinv,
       get = get, getinv = getinv)
}


## Computes the inverse of a matrix or returns a cached value if
## one is present

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if (!is.null(inv)) {
    return(inv)
  } else {
    inv <- solve(x$get(), ...)
    x$setinv(inv)
    return(inv)
  }
}

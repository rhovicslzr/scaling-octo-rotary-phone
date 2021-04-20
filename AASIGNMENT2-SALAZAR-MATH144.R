## Cache the Inverse of the matrix:
## Matrix inversion is commonly a exorbitant computation and there are few 
## advantages to cache the inverse of a matrix in comparison to compute it many times.
## Given are a pair of functions that are used in constructing a unique object that 
## stock a matrix and cache the corresponding inverse.

## This function will construct a unique "matrix" object that can cache the corresponding inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computates the inverse of the corresponding "matrix" constructed by 
## makeCacheMatrix above. If the inverse has already been obtained (and the 
## matrix is still the same), then it should bring back the corresponding inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Bring back a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
## A pair of functions that cache the inverse of a matrix

# Function to create a special "matrix" that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
  
    # Setter to update the matrix
    set <- function(y) {
      x <<- y
      inv <<- NULL  # Reset the inverse if the matrix changes
    }

    # Getter to retrieve the matrix
    get <- function() x

    # Setter to cache the inverse
    setInverse <- function(inverse) inv <<- inverse

    # Getter to retrieve the cached inverse
    getInverse <- function() inv

    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## Compute the inverse of the special matrix returned by "makeCacheMatrix"
## above. If the inverse has already been calculated (and the matrix has not
## changed), then the "cachesolve" should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    
    # Check if the inverse is already cached
    if (!is.null(inv)) {
      message("Getting cached inverse")
      return(inv)
    }
    
    # Compute the inverse if it's not cached
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setInverse(inv)
    inv
}

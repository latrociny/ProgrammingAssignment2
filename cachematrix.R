## The makeCacheMatrix helps to create a matrix. 
## Any matrix name can be used to store into the makeCacheMatrix
## e.g. my_matrix.
## There are more functions that are bulit inside and can be
## accessed similar to using a list e.g my_matrix$get() to get the matrix

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

# The cacheSolve function calculates the inverse of the matrix. 
## If the inverse has already been calculated, it will not calculate again
## But fetch the value from the cache. 
## Otherwise, it will calculate the inverse and store it in the setinverse function
## to be accessed in future via the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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

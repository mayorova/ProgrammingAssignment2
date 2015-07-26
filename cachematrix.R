## The following two functions provide a mechanism to cache the results
## of the R "solve" function (calculating the inverse of a matrix),
## which makes the code, where the inverse of matrix is used more than once,
## more efficient

## This function creates a special object, which stores the inversed matrix
## as a cache (in object called "inv"), and also provides methods to set and get
## the original matrix (methods"set" and "get" accordingly), and set and get the 
## inverse matrix (methods "setsolve" and "getsolve" accrodingly).
## Example of usage:
## To create a cached matrix execute the following:
## > cachedMatrix <-makeCacheMatrix(m)

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) inv <<- solve
  getsolve <- function() inv
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## This function is used to get the inverse of a matrix using R method "solve", but
## should be applied to a special matrix object created by previous function. When the 
## method is executed, it is first checked whether the cached value of the inverse matrix
## exists, and this value is returned directly (no calculations perfored). Otherwise, if the
## value is not found, it is calculated (using "solve" function), stored for future 
## accesses, and returned.
## Example of usage:
## For the matrix object created previously, to get the inverse the following is executed:
## > cacheSolve(cachedMatrix)
## If the above command is repeated, we'll se the "getting cached data" message, which means that
## the value has been taken from the cache, and is not calculated again

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getsolve()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setsolve(inv)
  inv
}

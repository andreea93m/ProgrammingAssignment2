## These functions help at computing the inverse of a matrix
## efficiently by caching its value.

## This function creates a cache matrix from a matrix that is passed as an argument

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inv2) inv <<- inv2
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## This function returns the inverse of the matrix. If it is already computed, it returns the
## cached inverse.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv
        ## Return a matrix that is the inverse of 'x'
}

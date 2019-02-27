# makeCacheMatrix() returns a list of functions regarding cached inverse.
# cacheSolve() checks the cache for inverse of the matrix and calculates it, if required.


# This function return 4 functions described below:
# 1. set() function updates the matrix. This function is to be used whenever the matrix is to be changed.
#    The function also deletes the cached value of the inverse, if any.
# 2. get() function is used to check the original matrix for which the inverse is cached/will be calculated.
# 3. setinv() function will get the inverse as whatever is passed as an argument.
#    This function should not be called anywhere apart from cacheSolve() function.
# 4. getinv() function returns the cached inverse of the matrix.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


# This function checks the cache for the inverse.
# If found, it will return it.
# Otherwise, it will calculate the inverse and return it after putting the value in the cache.
cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}


# To use the above code, create a makeCacheMatrix() object.
# Set the matrix using the get() function and calculate the matrix using cacheSolve() function.
mcm <- makeCacheMatrix()

m <- matrix(1:4, nrow = 2, ncol = 2)
mcm$set(m)
mcm$get()

mInverse <- cacheSolve(mcm)
mInverse

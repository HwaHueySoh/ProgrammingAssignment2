## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  # upon creation, no inverse is calculated, clear cache for inverse
  inv <- NULL
  # when changing the matrix, inverse in cache is set to null
  set <- function(y = matrix()) {
    x <<- y
    inv <<- NULL
  }
  # function to fetch the matrix
  get <- function() x
  # store inverse in cache when calculated
  setinv <- function(inverse) inv <<- inverse
  # retrieve cached inverse matrix
  getinv <- function() inv
  
  list( set = set, get = get,
        setinv = setinv,
        getinv = getinv )
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  # obtain cached inverse, if any
  inv <- x$getinv()
  # if cached inverse exists, use it
  if (!is.null(inv) ) {
    message("getting cached data")
    return(inv)
  }
  # if cached inverse does not exist, get matrix to calculate inverse
  data <- x$get()
  # calculate inverse
  inv <- solve(data, ...)
  # cache inverse
  x$setmean(inv)
  inv
}

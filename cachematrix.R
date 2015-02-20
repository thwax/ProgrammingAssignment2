## The following functions reduce computation efforts by caching the inverse of a matrix
## rather than to compute it repeatedly. They rely on R's lexical scoping.

## The first function returns a list of functions that enables the second function to compute
## the inverse of a (square) matrix using the solve() function. Some variables refer to the
## environment instantiated by the second function.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This second functions calculates and returns the inverse of the matrix created above
## using the solve() function. It begins by checking if the inverse has already been computed once.
## If so, it will use that calculation and return it, along with a notification message.
## If not, it takes in the matrix provided by makeCacheMatrix, then applies the solve()
## function and finally stores the result in the vector m and returns it.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  matrix <- x$get()
  m <- solve(matrix, ...)
  x$setinverse(m)
  m
}

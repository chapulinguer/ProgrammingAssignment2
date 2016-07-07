## makeCacheMatrix will create a matrix that can cache its inverse
## this function will set tha matrix, get the matrix, set the inverse 
## of the matrix and get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inver <- NULL
  set <- function(y) {
    x <<- y
    inver <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inver <<- inverse
  getInverse <- function() inver
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Function cacheSolve will solve the equation a %*% x = b for x, being b a matrix
##cacheSolve inverts the cacheMatrix created before. This function will check first
##if the cacheMatrix is already inverted. If this is the case it will get the 
##inversion of the matrix avoiding complex computation.


cacheSolve <- function(x, ...) {
  inver <- x$getInverse()
  if (!is.null(inver)) {
    message("getting cached data")
    return(inver)
  }
  mat <- x$get()
  inver <- solve(mat, ...)
  x$setInverse(inver)
  inver

}

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## Example - stores this matrix in cache:
##      [,1] [,2] [,3]
## [1,]  0.1  0.1  0.2
## [2,]  0.2  0.0  0.2
## [3,]  0.4  0.2 -0.4
##
## x <- makeCacheMatrix(matrix(c(.1,.2,.4,.1,0,.2,.2,.2,-.4),ncol=3,nrow=3))

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve returns the inverse matrix of a matrix that is stored in cache
## through makeCacheMatrix. This function assumes that the matrix is invertible
## (otherwise, it will return an error as per solve() function).

## Example - returns the invertible matrix of the cahced matrix done in the
## makeCacheMatrix example:
## cacheSolve(x)
##
## [,1] [,2] [,3]
## [1,]  0.1  0.1  0.2
## [2,]  0.2  0.0  0.2
## [3,]  0.4  0.2 -0.4

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

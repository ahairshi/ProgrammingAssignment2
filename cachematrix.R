## Put comments here that give an overall description of what your
## functions do

##The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
##set the value of the matrix - set(<matrix>)
##get the value of the matrix - get()
##set the inverse of the matrix - setinverse(<invmatrix>)
##get the inverse of the matrix - getinverse()

## Write a short comment describing this function
##This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  invMatrix <- NULL
  set <- function(y) {
    x <<- y
    invMatrix <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) invMatrix <<- inverse
  getinverse <- function() invMatrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## Write a short comment describing this function

##This function computes the inverse of the special "matrix" returned by 
#makeCacheMatrix above. If the inverse has already been calculated 
#then the cachesolve should retrieve the inverse from the cache.
##Computing the inverse of a square matrix can be done with the solve function in R. For example, if X is a square invertible matrix, then solve(X) returns its inverse.
##This function assumes that the matrix supplied is always invertible.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invMatrix <- x$getinverse()
  if(!is.null(invMatrix)) {
    message("getting cached data")
    return(invMatrix)
  }
  data <- x$get()
  invMatrix <- solve(data)
  x$setinverse(invMatrix)
  invMatrix
}
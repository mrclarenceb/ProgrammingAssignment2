## Put comments here that give an overall description of what your
## functions do

##OVERALL DESCRIPTION:These functions makeCacheMatrix and cacheSolve are used to create a matrix 
## (a list of functions to create a matrix), and compute & cache its inverse (assuming it is an invertible square matrix)
## Example: x <- matrix(c(1,0,5,2,1,6,3,4,0),nrow=3); cacheSolve(makeCacheMatrix(x))  ##returns cached invertible matrix




## Write a short comment describing this function
## makeCacheMatrix DESCRIPTION: This function accepts as input a matrix "x", and creates and caches a new matrix "m".
## The resulting output is a list of 'getter' and 'setter' functions used to get and set values to these matrices.


makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## Write a short comment describing this function

## cacheSolve DESCRIPTION: This function accepts as input a matrix "x", checks whether or not 
## its inverse exists in "cache" and if not computes its inverse, and finally returns its inverse "m".


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
  
}

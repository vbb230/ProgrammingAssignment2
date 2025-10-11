## The goal is to create and submit functions that (1) makeCacheMatrix(): creates a special "matrix" object that can store (cache) its inverse, (2)  cacheSolve(): computes the inverse of the matrix if it's not cached, or returns the cached result if it is. 

## https://github.com/vbb230/ProgrammingAssignment2

## It creates a  "matrix" object that can cache its inverse.It returns a list of functions to: set and get the matrix and set and get the inverse of the matrix

## Function 1: makeCacheMatrix: 
## Comment: 
## This function creates a special "matrix" object that can cache its inverse:

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(inverse) inv <<- inverse
  
  getinverse <- function() inv
  
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Function 2: cacheSolve
## Comment function 2
## This function computes the inverse of the special "matrix" created by makeCacheMatrix above.
## If the inverse has already been calculated, it retrieves the cached result. So, I need to do the following:

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  
  if (!is.null(inv)) {
    message("Getting cached inverse...")
    return(inv)
  }
  
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setinverse(inv)
  inv
}


## Use example 
# First, create a matrix
v <- matrix(c(1, 2, 3, 4), 2, 2)

# Then, it is necessaru to create a special matrix object
cachedMatrix <- makeCacheMatrix(v)

# Second, it computes and caches the inverse
cacheSolve(cachedMatrix)

# Third, it retrieves cached inverse
cacheSolve(cachedMatrix)



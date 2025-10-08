## Put comments here that give an overall description of what your


## Write a short comment describing this function
# It creates a  "matrix" object that can cache its inverse.It returns a list of functions to:
#  set and get the matrix
#  set and get the inverse of the matrix

## makeCacheMatrix: 
## This function creates a special "matrix" object that can cache its inverse.

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


## cacheSolve:
## This function computes the inverse of the special "matrix" created by makeCacheMatrix above.
## If the inverse has already been calculated, it retrieves the cached result.

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



m <- matrix(c(1, 2, 3, 4), 2, 2)
cm <- makeCacheMatrix(m)
cacheSolve(cm)
cacheSolve(cm)  # This call should say "Getting cached inverse..."

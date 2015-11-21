## This source code is designed to allow a user to create a matrix,
## create its inverse and then cache the matrix in an environment 
## variable.  It assumes that the matrix submitted is invertible

## This function allows a user to assign a matrix to it 
## It actually doesn't do all that much other than give 
## higher level functions that are somewhat globally named to 
## 'get' and 'set'.  The one powerful portion of this funcion is to
## NULL out m if a new matrix is created



makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve does a lot of work.  it takes a matrix defined from makeCacheMatrix
## (or really from anywhere), associates the inverse of that matrix to
## a global variable 'm' and then anytime you call it again, if m has not been
## nulled out by the creation of a new matrix in makeCacheMatrix
## it will return M again or it will recalculate

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  inverse <- x$get()
  m <- solve(inverse, ...)
  x$setinverse(m)
  m
}  


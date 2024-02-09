## Put comments here that give an overall description of what your

#makeCacheMatrix consists of set, get, setinv, getinv


makeCacheMatrix <- function(x = matrix()) {         #set the input x as a matrix
  p <- NULL                                         #initializing p as null
  set <- function(y) {
    x <<- y
    p <<- NULL
  }
  get <- function() x                               #function to get the matrix x
  setinverse <- function(inverse) p <<- inverse
  getinverse <- function() p
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

cacheinverse <- function(x, ...) {                  #getting cache data
  p <- x$getinverse()
  if(!is.null(p)) {                                 #checking if p is null
    message("getting cached data")
    return(p)                                       #return the p value 
  }
  matrix_to_invert <- x$get()
  p <- solve(matrix_to_invert, ...)                 #calculates p value
  x$setinverse(p)
  p                                                 #p equals the inversed x matrix 
}

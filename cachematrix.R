## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## creates a special "matrix" object that can cache its inverse

## creates a special "matrix", 

makeCacheMatrix <- function(x = matrix()) {
    im <- NULL
    set <- function(y) {
    x <<- y
    im <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) im <<- inverse
  getinverse <- function() im
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
## computes the inverse of the special "matrix" returned by
## makeCacheMatrix above.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    cacheSolve <- function(x, ...) {
    im <- x$getinverse()
    if(!is.null(im)) {              ## If the inverse has already been calculated (and the
                                    ## matrix has not changed),return the result
      message("getting cached data")
      return(im)
    }
    
    data <- x$get()               ## get the matrix to compute the inverse of the matrix 
    im <- solve(data, ...)        ## use solve() caculating the inverse of the matrix
    x$setinverse(im)
    im
  }
}

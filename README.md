ProgrammingAssignment2
======================

Caching the Inverse of a Matrix
###this function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  ## set value of matrix
  i<- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  ## get value matrix
  get <- function() x
  
  ## set inverse matrix
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  
  ## get inverse matrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

##this function computes the inverse of the special "matrix" returned by makeCacheMatrix.
##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache
cacheSolve <- function(x, ...) {
  ## return matrix that is inverse of x
  
  ## get inverse of matrix        
  i <- x$getinverse()
  
  ## check if matrix exists   
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  ## if does not exist get inverse of matrix   
  data <- x$get()
  i <- solve(data, ...)
  ## set the inverse of the matrix 
  x$setinverse(i)
  i
}




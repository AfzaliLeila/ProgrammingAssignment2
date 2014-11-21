
##this function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(m = matrix()) {
  ## Initialize value of matrix
  i<- NULL
  ##method to set the matrix		
  set <- function(matrix) {
    m <<- matrix
    i <<- NULL
  }
  ## method to get value matrix
  get <- function() {
	## return matrix
	m
}
  
  ## method to set inverse matrix
  setInverse <- function(inverse){
 	i<<- inverse
}
 ## method to get inverse matrix
  getInverse <- function() {
	##return inverse
	i
  }
  ##Return methods 
  list(set = set, get = get, setInverse = setInverse,  getInverse = getInverse)
}




is function computes the inverse of the special "matrix" returned by makeCacheMatrix.
##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache
cacheSolve <- function(x, ...) {
  ## return matrix that is inverse of x
  m  <- x$getInverse()
   
  ## check if matrix exists  and get inverse of matrix 
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
 
  ##get matrix from object 
  data <- x$get()
 
  ##calculate inverse - matrix multiplication
  m <- solve(data) %*% data

  ## set the inverse to object 
  x$setinverse(m)

  ## return matrix
  m
}


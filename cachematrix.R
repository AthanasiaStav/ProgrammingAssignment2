## The following functions cache the insverse of a matrix



## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
      ## Initialize the value of the inverse matrix to NULL
      m <- NULL
      ## Use the set function either for the first time the matrix is cached or when 
      ## changes have been made to the matrix.
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
     ## Get the value of the matrix
    get <- function() x
    ## Set the inverse of the matrix
    setinverse <- function(inverse) m <<- inverse
    ## Get the inverse of the matrix
    getinverse <- function() m
    
    ## Create a list thet is going to be passed to the cacheInverse function so 
    ## as to get the inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
  
}



##  This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##  If the inverse has already been calculated (and the matrix has not changed), 
##  then the cachesolve should retrieve the inverse from the cache.

cacheInverse <- function(x, ...) {
      
      ## Check if the inverse has already been calculated (and the matrix has not changed), 
      ##  then the cachesolve should retrieve the inverse from the cache and end the function
      ## (return function)
      m <- x$getinverse()
         if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    
    ## if the inverse has not already been calculated or the matrix has changed
    ## get the data of the matrix and compute the inverse of the matrix.
    data <- x$get()
    m <- solve(data, ...)
    ## Set the inverse of the matrix as m
    x$setinverse(m)
    m

}

## These functions compute the inverse of a matrix, cache the value of the inverse,
## and allow for the retrieval of the inverse without recalculating it. 
## This method can save time over repeated use of solve(x), for large matricies.


##This function creates an object that contains a matrix and it's inverse. 
##The function contains methods for setting and getting the inverse.
##The object created by this function can be combined with CacheSolve
##to quickly return the cached matrix inverse.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() {x}
  setMatrixInverse <- function(MatrixInverse) m <<- MatrixInverse
  getMatrixInverse <- function() m
  list(set = set, get =get, 
       setMatrixInverse = setMatrixInverse, 
       getMatrixInverse = getMatrixInverse)

}


##This function can take as its input, the object created by makeCacheMatrix.
##The function will will return the inverse of the matrix input into makeCacheMatrix, 
##even if the inverse is not already cached.
cacheSolve <- function(x, ...) {
  m <- x$getMatrixInverse() ##go fetch m, which will either be Null OR the inverse of a matrix
  if(!is.null(m)) {         ##If m is not Null, then retrieve the cached value.
    message("Getting cached data")
    return(m)
  }
  data <- x$get()         ##We only get here if the value of m was Null, meaning that the
                          ## inverse was not calculated yet. This uses the get method from
                          ## makeCacheMatrix and grabs the matrix that was input into makeCacheMatrix.
  m <- solve(data)        ## Since it is not cached already, we need to find the inverse of the 
                          ## matrix in makeCacheMatrix.
  x$setMatrixInverse(m)   ## This creates the cached version of the inverse and allows it to be 
                          ## retrieved next time the function is called, without recalculating it.
  m                       ## the inverse matrix is returned
}




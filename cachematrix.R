## Functions to Caching the Inverse of a Matrix

## Function: Create a special matrix object that can cache its inverse
## 3 sub-functions exists: 
##    1. set: set the Matrix
##    2. get: get the Matrix 
##    3. getInv: compute the inverse of the Matrix if it is not done already.
##              if it is already done, just get it from the cache.

makeCacheMatrix <- function(X = matrix()) {

    if(nrow(X) != ncol(X)){
      message("Input matrix is not square. Initiation aborted.")
      return(NULL)
    } 
    Xinv <- NULL
    set <- function(Y) {
      if(nrow(Y) != ncol(Y)){
        stop("Input matrix is not square. Setting aborted.")
      }
      X <<- Y
      Xinv <<- NULL
    }  
    get <- function() X
    setInv <- function(Yinv) Xinv <<- Yinv
    getInv <- function() Xinv
    
    list(set = set,
         get = get,
         setInv = setInv,
         getInv = getInv)
  
}


## Compute the inverse of the special matrix from the function above

cacheSolve <- function(X, ...) {
    ## Return a matrix that is the inverse of 'X'

    Xinv <- X$getInv()
    if(!is.null(Xinv)) {
    message("getting cached data")
    return(Xinv)
    }
    Y <- X$get()
    Yinv <- solve(Y, ...)
    X$setInv(Yinv)
    Yinv

}

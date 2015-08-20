## These functions allow creation of a matrix, along with its inverse.
## If the inverse of the matrix has already been created/cached,
## the cached inverse will be retrieved instead of re-calculating the inverse

## creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  #initialize inverse to null
  inverse <- NULL
  
  #initialize the matrix to the passed value, and its inverse to null
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  #return the matrix
  get <- function() x
  #cache the matrix's inverse
  setInverse <- function(x) inverse <<- x
  #retrieve the cached inverse
  getInverse <- function() inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  #retrieve inverse of x (might be null)
    i<-x$getInverse()
    #if inverse is not null(i.e. it was cached)
    if (!is.null(i)){
      #return the cached inverse
      message("getting cached data")
      return(i)
    }
    #get x, to calculate its inverse
    data<-x$get()
    #solve the inverse, since it wasn't cached
    i<-solve(data,...)
    #cache the inverse
    x$setInverse(i)
    #return the inverse
    i
}

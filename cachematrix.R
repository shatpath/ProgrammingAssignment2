## makeCacheMatrix and cacheSolve functions caches the value of a matrix
## makeCacheMatrix function is invoked first by passing a  numeric square matrix
## cacheSolve function is invoked next by passing the object returned by makeCacheMatrix
## cacheSolve returns inverse of the matrix from cache or computing it if not found in cache 



## makeCacheMatrix caches the value of the numeric square matrix which was passed as an argument to it. 
## It returns a list of objects and defines four internal functions as below
## Internal Functions :
## get : Returns the matrix object which was passed as arguement to makeCacheMatrix   
## set : Sets the new matix passed as argument and resets the cache object to null
## getMatrix : returns the cached matrix object 
## setMatrix : Cache the argument matrix 

makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  
  setMatrix <- function(invMat) i <<- invMat
  
  getMatrix <- function() i
  
  list (set=set, get = get,setMatrix = setMatrix,
        getMatrix = getMatrix)
  
  
}


## cacheSolve returns a matrix that is inverse of x
## It is invoked by passing the return object from makeCacheMatrix function.
## It returns the inverse of the square matrix by first looking it in cache, 
## and returns the same if it cache. If it doe not exist in cache then it computes 
## the inverse and caches the value by invoking setMatrix() and then returning the 
## inverse matrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getMatrix()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  
  class(data)
  i <- solve(data, ...)
  x$setMatrix(i)
  i	
}
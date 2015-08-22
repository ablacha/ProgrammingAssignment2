## Put comments here that give an overall description of what your
## functions do

# makeCacheMatrix creates a special matrix, which in fact is a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse matrix
# 4. get the vaue of inverse matrix

# cacheSolve calculates inverse matrix of the special matrix as defined in makeCacheMatrix function.
# This function checks first if the inverse has already been calculated. If it was then it gets the inverse matrix
# from the cache and skips calculation. Otherwise it calculates the inverse matrix and sets the value of the inverse matrix
# in the cache via the setInverse function.


## Write a short comment describing this function
#create a special matrix for caching
makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  
  #set the value of the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  #get the value of the matrix
  get <- function() x
  
  #set the value of the inverse matrix
  setInverse <- function(solve) m <<- solve
  
  #get the value of the inverse matrix
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function
#returns inverse of the matrix (or returns cached inverse matrix if it is available)
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  #get cached inverse matrix
  m <- x$getInverse()
  
  #if cached then return cached inverse matrix
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  #if not cached then get matrix
  data <- x$get()
  
  #calculate inverse
  m <- solve(data, ...)
  
  #set inverse matrix 
  x$setInverse(m)
  m
}

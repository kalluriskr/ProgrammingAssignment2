## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## Programming Assignment 2: Caching the Inverse of a Matrix

# Assumptions:  1 the matrix supplied is always an invertible matrix .
# The first function makeCacheMatrix creates a special "matrix", which is really a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse of the matrix using solve()
# 4. get the value of the inverse of the matrix using solve()

makeCacheMatrix <- function(inputMat = matrix()) {
  returnMat <- NULL
  set <- function(storeMat) {
    inputMat <<- storeMat
    returnMat <<- NULL
  }
  get <- function() inputMat
  setInv <- function(solve) returnMat <<- solve
  getInv <- function() returnMat
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


# The following function cacheSolve calculates the inverse of the special "matrix" created with 
# the above function (makeCacheMatrix). However, it first checks to see if the inverse of the function has already been 
# calculated. If so, it gets the inverse of the function from the cache and skips the computation. 
# Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setInv function.

cacheSolve <- function(inputMat, ...) {
## Return a matrix that is the inverse of 'inputMat'
  storeMat <- inputMat$getInv()
  if(!is.null(storeMat)) {
    message("getting cached data")
    return(storeMat)
  }
  data <- inputMat$get()
  storeMat <- solve(data, ...)
  inputMat$setInv(storeMat)
  storeMat
}

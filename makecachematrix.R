makeCacheMatrix <- function(A = matrix())
{
  S <- NULL
  set <- function(B)
  {
    A <<- B
    S <<- NULL
  }
  get <- function() A
  setinv <- function(B) S <<- B
  getinv <- function() S
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


cacheSolve <- function(A, ...)
{
  S <- A$getinv()
  if (is.null(S)) {
    message('Computing inverse...')
    data <- A$get()
    S <- solve(data, ...)
    A$setinv(S)
  } else {
    message('Returning cached inverse...')
  }
  return(S)
}
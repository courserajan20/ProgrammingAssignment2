## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv_matrix <- NULL
  
  set <- function(y) {
    x <<- y
    inv_matrix <<- NULL
  }
  
  get <- function() x
  
  setMatInverse <- function(inverse_mat) inv_matrix <<- inverse_mat
  
  getMatInverse <- function() inv_matrix
  
  
  list(set = set, get = get,
       setMatInverse = setMatInverse,
       getMatInverse = getMatInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  # first check in cache, if avil
  inv_matrix <- x$getMatInverse()
  
  if(!is.null(inv_matrix)) {
    message("getting cached data")
    return(inv_matrix)
  }
  
  # inverse not found, now get the matrix
  data <- x$get()
  
  # calculate the inverse of the matrix
  inv_matrix <- solve(data, ...)
  
  # set the calculated inverse to cache
  x$setMatInverse(inv_matrix)
  
  # returning the inv of matrix asked
  inv_matrix
}

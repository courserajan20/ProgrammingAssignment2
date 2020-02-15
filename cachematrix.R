## Put comments here that give an overall description of what your
## functions do

## `makeCacheMatrix`` manages the caching of the inverse and returns
## to the 2nd function when asked. The 2nd func `cacheSolve` replies 
## with the cached inverse of matrix if exists or calculates it, gives 
## the cache and returns the same to the user.

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  # var init.
  inv_matrix <- NULL  
  
  set <- function(y) {
    x <<- y
    inv_matrix <<- NULL
  }
  
  # return the original matrix
  get <- function() x
  
  # seting the inv to cache
  setMatInverse <- function(inverse_mat) inv_matrix <<- inverse_mat
  
  # return the cached inv matrix
  getMatInverse <- function() inv_matrix
  
  
  list(set = set, get = get,
       setMatInverse = setMatInverse,
       getMatInverse = getMatInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  # ask if the inverse exists
  inv_matrix <- x$getMatInverse()
  
  if(!is.null(inv_matrix)) {
    message("getting cached data")
    
    # matrix inverse found, returning
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

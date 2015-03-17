## Creating a special matrix for caching.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  get <- function() x
  set_inverse <- function(inverse) i <<- inverse
  get_inverse <- function() i
  
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}

## Returning the inverse matrix by getting it from the cache or calculating it, 
## depends on if it is calculated before.

cacheSolve <- function(x, ...) {
  inverse_x <- x$get_inverse()
  
  if(!is.null(inverse_x)) {
    message("Getting cached data.")
    
    return(inverse_x)
  }
  
  matrix <- x$get()
  inverse_x <- solve(matrix)
  x$set_inverse(inverse_x)
  
  return(inverse_x)
}

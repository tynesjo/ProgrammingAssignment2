# This program contains two functions:
# 'makeCacheMatrix' which creates a special matrix object with a "cache"
# that records the result of its inversion (if called before) in the form
# of a list.



# This function creates a list object which contains a matrix as well as
# functions to handle a simple caching method for computing its inverse,
# through assignment to the global environment. 
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL

  list(
    set		= function(y)		{x <<- y; i <<- NULL},
    get		= function()		{x},
    setinv	= function(inv)		i <<- inv,
    getinv	= function()		{i}
  )
}


# This function takes a "special" matrix object with a cached result of
# its inversion as argument, contained in a list as created by the
# makeCacheMatrix function and returns the result of inverting the matrix
# either by performing the computation (in case the result is not contained
# in the list cache) or by retrieving the result from the list cache.

cacheSolve <- function(x, ...) {
  i <- x$getinv()

  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }

  data	<- x$get()
  j	<- solve(data, ...)
  x$setinv(j)
  j -> out
}

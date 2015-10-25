## Together, these two functions take the value of a matrix, set it to a global variable 
## accessible to other functions, perform an inverse function on the matrix, 
## then save the result as a global value.


###############################################################
## Pass a matrix as an argument to the function in order to set 
## the values of the matrix as global values.

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL ##set local variable to null
  set <- function(y) { 
    x <<- y ## set x to the value passed by the argument globally
    m <<- NULL ##set global variable to null
  }
  get <- function() x ## pass the matrix into the function
  setInverse <- function(invert) m <<- invert ## value passed to this function becomes global 
  getInverse <- function() m ## current value of the m variable (initially null)
  list(set = set, get = get, ## sets subsets of the values entered into the function
       setInverse = setInverse,
       getInverse = getInverse)

}

##########################################################################
## Create a matrix that is the inverse of the one passed into the function

cacheSolve <- function(x, ...) {
        
  m <- x$getInverse() ## set value for getInverse function within the makeCacheMatrix function
  if(!is.null(m)) { ## only happens when there's an existing value for the matrix
    message("getting cached data")
    return(m) ## returns previous-calculated value
  }
  ## when there isn't an existing matrix value or a new matrix is passed into the function, 
  ## create the inverse and pass that value to makeCacheMatrix 
  data <- x$get() ## set local variable from the value passed into makeCacheMatrix  
  m <- solve(data, ...) ## calculates the inverse matrix
  x$setInverse(m) ## pass the value calculated back into the function within makeCacheMatrix
  m ## returns new value for m variable local to this function; global value remains the same
}

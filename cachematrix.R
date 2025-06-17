## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# Function to create a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
   inv <- NULL # Initialize the inverse as NULL
   
   # Function to set a new matrix
   set <- function(y) {
     x <<- y
     inv <<- NULL # Reset the inverse when a new matrix is set
   }
   
   # Function to get the matrix
   get <- function() x
   
   # Function to set the inverse
   setInverse <- function(inverse) inv <<- inverse
   
   # Function to get the inverse
   getInverse <- function() inv
   
   # Return a list of the functions
   list(set = set,
        get  get,
        setInverse = setInverse,
        getInverse = getInverse)
}

# Function to compute or retrieve the cached inverse of the matrix
cachesolve <- function(x, ...) {
  inv <- x$getInverse() # check if Inverse is already cached
  
  # If cached, return it
  if (!is.null(inv)) {
    message("Getting cached inverse")
    return(inv)
  }
  
  # If not cached, compute the inverse
  mat <- x$get()
  inv <- solve(mat, ...) # Compute the inverse
  
  x$setInverse(inv) # Cache the inverse
  inv # Return the inverse
}

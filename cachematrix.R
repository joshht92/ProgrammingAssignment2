install.packages('tidyverse')
library(tidyverse)

## This program creates a cache that stores value of your calculation. If the function is called
#it will return the cached value. If the dataset changes it will create a new cached value and store.


##Creates a list containing functions to interact with a matrix in a way that allows for caching
makeCacheMatrix <- function(x = matrix()) {
  ## This variable is intended to cache/store the result of an operation on the matrix x.
  m <- NULL
  
  ##updates x with a new matrix y and resets m to NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ##Returns the matrix x
  get <- function() x
  
  ##Assigns the computed result (passed as solve) to m
  setsolve <- function(solve) m <<- solve
  ##Returns the value stored in m
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)

}


##Computes the inverse of a matrix that is within the list structure returned by makeCacheMatrix.
cacheSolve <- function(x, ...) {
  ##Attempts to retrieve the cached inverse of the matrix from x
  m <- x$getsolve()
  ##Checks if m is not NULL, meaning the inverse has already been calculated and cached.
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ##Retrieves the matrix from x using the get function
  data <- x$get()
  ##Computes the inverse of the matrix using the solve function
  m <- solve(data, ...)
  ##Stores the computed inverse in the cache by calling the setsolve function of x.
  x$setsolve(m)
  m
}


B <- matrix(c(1,42,5,6),2,2)
B1 <- makeCacheMatrix(B)
cacheSolve(B1)





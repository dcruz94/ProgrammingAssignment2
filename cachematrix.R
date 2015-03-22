# Author: Cruz Davalos Diana Ivette
# Version: 2.0
# Date: March 22, 2015

##****************************************************************************************
## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly.
## -----------------------------------------------------------------------------------
## Functions reported here:
## -----------------------------------------------------------------------------------
## makeCacheMatrix: This function creates a special "matrix" object that can cache 
## its inverse.
## ..................................................................................
## cacheSolve: This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix 
## has not changed), then the cachesolve should retrieve the inverse from the cache.
## ------------------------------------------------------------------------------------
## Both functions take the structure provided by the examples in the Assignment 2.
##**************************************************************************************** 

# Starts makeCacheMatrix() taking a matrix class variable as argument
# and returning the 'special' matrix
makeCacheMatrix <- function(x = matrix()) {
  # Initialize the variable that will hold the inverse matrix
  mat <- NULL

  # Function that assigns the value of the matrix,
  # even when 'x' comes from other environment.
  # It is also important to reset 'm', because calling
  # this function means that the value of the original matrix
  # has changed.
  setmatrix <- function(y) {
    x <<- y
    mat <<- NULL
  }
  
  # Gets the original matrix 'x'
  getmatrix <- function() x

  # Assign the inverse matrix to 'mat' for using it after
  setInverse <- function(solve) mat <<- solve

  # We can call the value of mat because of the '<<-' assignation operator
  # used before, so we do not have to calculate the inverse matrix again,
  # we just have to call it.
  getInverse <- function() mat

  # That's it! Return all that you need, the set functions to assign
  # values if necessary, and the get functions to call the cached values.

  list(setmatrix = setmatrix, getmatrix = getmatrix,
       setInverse = setInverse,
       getInverse = getInverse)
}


# Starts cacheSolve() function taking the 'special' matrix as argument
# and returning the inverse matrix 'm'
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  #This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
  #If the inverse has already been calculated (and the matrix has not changed), 
  #then the cachesolve should retrieve the inverse from the cache.

  # Verifies if the inverse has already been calculated
  # if so, it returns the cached inverse
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }

  # If the inverse matrix hasn't been calculated, 
  # it gets the inverse with the 'solve()' function
  # and caches by calling the 'setInverse()' function wit 
  # the inverse matrix 'm' as argument.	
  data <- x$getmatrix()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}


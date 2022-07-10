## Put comments here that give an overall description of what your
## functions do
## A pair of functions that cache and Solve the inverse of a matrix

## Write a short comment describing this function
## This function creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  ## Initialize the inverse with NULL
  inverse <- NULL
  
  ## Method to set the matrix
  setmatrix <- function(matrix) {
    m1 <<- matrix
    inverse <<- NULL
  }
  
  ## Method the get the matrix
  getmatrix <- function() {
        m1
    ## Return the matrix
  }
  
  ## Method to set the inverse of the matrix
  setInverse <- function(inverse2) {
    inverse <<- inverse2
  }
  
  ## Method to get the inverse of the matrix
  getInverse <- function() {
    ## Return the inverse 
    inverse
  }
  
  ## Return a list of the methods
  list(setmatrix = setmatrix, getmatrix = getmatrix,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Compute the inverse of the matrix returned by "makeCacheMatrix".
## If the inverse has already been calculated (and the matrix has not
## changed), then the "cachesolve" should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    matrix <- x$getInverse()
    
    ## Just return the inverse if its already set
    if( !is.null(matrix) ) {
      message("getting cached data")
      return(matrix)
    }
    
    ## Get the matrix from our object
    data <- x$getMatrix()
    
    ## Calculate the inverse using matrix multiplication
    matrix <- solve(data) %*% data
    
    ## Set the inverse to the object
    x<-setInverse(matrix)
  
    ## Return the matrix
    matrix
  }

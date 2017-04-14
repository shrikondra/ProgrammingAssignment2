## This function cache the Inverse of a Matrix:
## Matrix inversion is done using solve(x, ...)
## This avoids calculating the inverse repeatedly.
## This is Week 3 assignment
## Steps to execute this functions are
## 1) Create a Matrix using makeCacheMatrix
## 2) Using cacheSolve calculate the inverse and set the inverse using setInverse
## 3) Using getInverse get the inverse from the cache whenever required


## This function makes the matrix which can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  # Here value of the matrix is set using x$set command
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # The values of matrix is retrieved using the x$get command
  get <- function() x
  
  # The inverse of matrix is set into the cache using x$setInverse command
  setInverse <- function(inverse) inv <<- inverse
  
  # The inverse of matrix is retrieved from the cache using x$getInverse command
  getInverse <- function() inv
  
  # Returns list 
  list(set = set,  get = get, setInverse = setInverse,  getInverse = getInverse)
  
}




## This function calculates inverse if not calculated else returns the inverse from the cache of the matrix object
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  # Check if Inverse is already calculated
  inv <- x$getInverse()
  
  # If inverse already in cahe return the inverse
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # If not calculated retrieve the matrix
  mat <- x$get()
  
  # Calculate the inverse using solve
  inv <- solve(mat, ...)
  
  # Set the inverse in cache
  x$setInverse(inv)
  
  # Return inverse of the matrix
  inv
}


# Test Example - Run all the steps in order
M <- makeCacheMatrix(matrix(sample(3*3),3,3)) # create random 3x3 square matrix
M$getInverse() # Should return NULL
cacheSolve(M) # find the inverse of matrix 
cacheSolve(M) # gets the inverse of matrix from cache
M$getInverse() # Should return the inverse now







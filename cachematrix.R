## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## makeCacheMatrix returns a list that contains a function to set and retrieve a matrix, and it's inverse
makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix <- NULL
  set <- function (y){
    x <<- y
    inverseMatrix <<- null
  }
  get <- function() {x}
  setInverseMatrix <- function(solve) {inverseMatrix <<- solve}
  getInverseMatrix <- function() {inverseMatrix}
  list(set = set, get = get,
       setInverseMatrix = setInverseMatrix,
       getInverseMatrix = getInverseMatrix)
}


## Write a short comment describing this function

## cacheSolve first checks if in this special matrix structure, there exists the inverse of the matrix. 
## If so, it retrieves the cache version which is stored under the getInverseMatrix.
## Else it calls on the setInverseMatrix function and saves it as part of the list. Next time, the function 
## returns the cached version.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverseMatrix <- x$getInverseMatrix()
  if(!is.null(inverseMatrix)) {
    message("getting cached data")
    return(inverseMatrix)
  }
  data <- x$get()
  inverseMatrix <- solve(data, ...)
  x$setInverseMatrix(inverseMatrix)
  inverseMatrix
}

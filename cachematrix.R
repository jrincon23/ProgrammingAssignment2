## Functions to cache a matrix inverse


##Creates an special "matrix" object able to cache its inverse
##parameters: x of type matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL #initialize the inverse inside the object
  ##defines the set function to change the matrix inside if needed
  set <- function(y){ 
    x<<-y
    i<<- NULL
  }
  ##defines the get function to retrieve the matrix
  get <- function() x
  ##defines the setinverse function to set the i variable (inverse)
  setinverse <- function(inverse) i<<- inverse
  ##defines the getinverse function to retrieve the inverse of the matrix
  getinverse <- function() i
  ##returns the list of functions of the object
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}



##Computes the inverse of the special "matrix"
##parameters: x of type "matrix" (created with makeCacheMatrix)

cacheSolve <- function(x, ...) {
  ##sets the i variable with the inverse inside the "matrix" object
  i <- x$getinverse()
  ##checks if inverse has already been calculated, if it has been calculated
  ##returns the inverse and leaves the function
  if(!is.null(i)){
    message("getting cached data for the matrix inverse")
    return(i)
  }
  ##if condition wasn't true it continues to get the matrix and use the solve
  ##function to get the inverse along with the parameters passed on the ...
  mat <- x$get()
  i <- solve(mat, ...)
  ##sets the inverse inside the "matrix" object
  x$setinverse(i)
  ##return the inverse
  i
}

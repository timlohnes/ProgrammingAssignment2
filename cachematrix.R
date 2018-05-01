## Put comments here that give an overall description of what your
## functions do
## Two functions, the first of which allows you to set or get the data and set or get the inverse of supplied matrix


## Write a short comment describing this function
## makeCacheMatrix returns a list of functions that can be used to set a matrix of values, get a matrix of values, 
## set the inverse of the matrix or get the inverse of a matrix, if it exists
## Any of the functions returned by makeCacheMatrix can be called on their own after running. 
## ie. a <- matrix(c(2, 4, 4, 2), nrow=2, ncol=2)
## b <- makeCacheMatrix(a)
## b$get() #retrieve the value of the previously supplied matrix, 2, 4, 4, 2
## b$set(matrix(c(1, 2, 2, 1), nrow = 2, ncol = 2)
## b$get() #retrieves the most recent value of x that was passed in line above, 1,2,2,1

makeCacheMatrix <- function(x = numeric()) {
  aInvMatrix <- NULL
  set <- function(y) {
    x <<- y
    aInvMatrix <<- NULL
  }
  get <- function() x
  setInvMatrix <- function(solve) aInvMatrix <<- solve
  getInvMatrix <- function() aInvMatrix
  list(set = set, get = get, setInvMatrix = setInvMatrix, getInvMatrix = getInvMatrix)
}


## Write a short comment describing this function
## cacheSolve passes a set of functions to see if the matrix is already cached. Retrieve the inverse matrix. 
## If it exists, return it, if it doesn't, set it and return it
cacheSolve <- function(x, ...) {
  aInvMatrix <- x$getInvMatrix()
  print(aInvMatrix)
  #message(paste("1st", invMatrix, sep = " "))
  if(!is.null(aInvMatrix)) {
    message("getting inverse matrix")
    return(aInvMatrix)
  }
  data <- x$get()
  aInvMatrix <- solve(data, ...)
  x$setInvMatrix(aInvMatrix)
        ## Return a matrix that is the inverse of 'x'
  aInvMatrix
}

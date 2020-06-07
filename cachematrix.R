## makeCacheMatrix will create a matrix 

makeCacheMatrix <- function(x = matrix()) { ## Define the argument with default mode of "matrix"
  a <- NULL          ## Initialize a as NULL; will hold value of matrix inverse which will be returned as output
  set <- function(b) {
    x <<- b
    a <<- NULL  ## If there is a new matrix, reset a to NULL
  }
  get <- function() x     ## The get fucntion - returns value of the matrix argument
  setInv <- function(inv) a <<- inv
  getInv <- function() a    ## Gets the value of a where called
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}

## cacheSolve will compute an inverse of the matrix inputted

## If the inverse of the matrux has already been computed (and the matrix has not changed),
## then cacheSolve will retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  a <- x$getInv()
  if(!is.null(a)){
    message("Getting Cached Matrix")
    return(a)
  }
  matrix <- x$get()
  a <- solve(matrix,...)
  x$setInv(a)
  a
}

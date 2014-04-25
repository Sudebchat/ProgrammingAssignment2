## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache
## its inverse

makeCacheMatrix <- function(x = matrix()) {

  len  <- length(x)   ## calculate the total number of elements
  lenS <- sqrt(len)   ## assuming the matrix is square and invertible
                      ## calculate the number of rows and columns
  Inv  <- NULL        ## initialize the inverse of the matrix as NULL
  
  ## Function part 1: Setting the value of the matrix
  set <- function(y) {
    x <- matrix(y, lenS)
    Inv <<- NULL      ## Initializing the inverse of the matrix to be
                      ## globally available
  }
  
  ## Function part 2: gettting the value of the matrix
  get <- function() x
  
  ## Function part 3: setting the value of the inverted matrix
  ## making it globally avaiable
  setsolve <- function(solve) Inv <<- solve
  
  ## Function part 4: getting the value of the inverted matrix
  getsolve <- function() Inv
  
  ## Creating the special vector(list) containing the function to
  ## get/set the original matrix and get/set the corresponding 
  ## inverted matrix
  list(set = set,  get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Write a short comment describing this function

## This function computes the inverse of the special "matrix"
## created by the function makeCacheMatrix().
## If the inverse has already been calculated (and the matrix
## has not changed), then cachesolve() retrieves the inverse from
## the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  ## Check for the avaiability of the inverted matrix for x
  Inv  <- x$getsolve()
  if(!is.null(Inv)) {               ## if the value is not NULL
    message("getting cached data")  ## then inform that the value was in cache
    return(Inv)                     ## and return the inverse of the matrix
  }
  
  ## Calculate the inverse of x not seen in cache
  data <- x$get()          ## get the matrix value
  Inv <- solve(data, ...)  ## calculate the inverse
  x$setsolve(Inv)          ## set the special vector with new inverse
  Inv                      ## return the inverse matrix
}

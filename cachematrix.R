## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix takes a matrix vector and saves in the private variable x

makeCacheMatrix <- function(x = matrix()) {
    
    #intialize the inverse matrix to NULL at makeCacheMatrix construction time
    m <- NULL
    
    #use <<- operator to set the value of x and m in specific set environment scope
    set <- function(y) {
    x <<- y
    m <<- NULL
  }
    
    #just a stardard get function that return value of fuction x
  get <- function() x
    
    # set the inverse matrix 
  setsolve <- function(solve) m <<- solve
    
    # return/get the inverse matrix
  getsolve <- function() m
    
    # create a list of operations
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
           message("getting cached data")
           return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}

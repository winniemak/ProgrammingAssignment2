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
    
    # create a list of functions
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)

}


#cacheSolve takes a cached Vector created with makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
            
           # if m is not NULL, then we already have a cached value that we can returned
           message("getting cached data")
           return(m)
        }
    
        # get() function to get the matrix vector
        data <- x$get()
    
        # calculate inverse matrix and pass to cacheSolve   
        m <- solve(data, ...)
    
        # once we have the inverse matrix value and set it into the cache
        x$setsolve(m)
    
        # returned cached inverse matrix
        m
}

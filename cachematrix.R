
## This function creates a matrix object that can cache its inverse

     ## Initialise key objects m and x
makeCacheMatrix <- function(x = matrix()) {
     m<- NULL
     ## Define the set function
     set<- function(y) {
          x<<- y
          m<<- NULL
     }
     ## Define the get function
     get<- function() x
     
     ## Define setmatrix and getmatrix functions
     setmatrix<- function(solve) m <<- solve
     getmatrix<- function() m
     
     ## Create a list with the previously defined set and get functions
     list(set=set, get=get, setmatrix=setmatrix, getmatrix=getmatrix)
}

## This function computes the inverse of the matrix object above
## If the inverse has already been calculated, it retrieves it from cache

cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
     m<- x$getmatrix()
     
     ## If the inverse has been calculated, retrieve it from cache
     if(!is.null(m)) {
          message("getting cached data")
          return(m)
     }
     
     ## If the inverse has not been calculated, calculate with solve function
     data<- x$get()
     m<- solve(data, ...)
     x$setmatrix(m)
     
     ## print the inverse matrix
     m
}
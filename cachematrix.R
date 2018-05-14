## Overall description
##
## - makeCacheMatrix creates a matrix
##
## - cacheSolve returns the inverse of the matrix

## makeCacheMatrix creates a matrix and returns a list of 4 functions:
## set(x) -- set the value of the matrix
## get() -- retrieve the value of the matrix
## setinv(x) -- set the inverse of the matrix
## getinv() -- retrieve the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
        
        x.inv <- NULL
        set <- function(y){
                x <<- y
                x.inv <<- NULL
        }
        
        get <- function() x
        setinv <- function(inv) x.inv <<- inv
        getinv <- function() x.inv
        
        list(set = set, get = get, 
             setinv = setinv, getinv = getinv)

}


## cacheSolve return the inverse of a makeCacheMatrix type matrix:
## - if the inverse has already been computed cacheSolve retrieves and returns the inverse
## - if the inverse has not been computed, cacheSolve computes it, store it in makeCacheMatrix and returns the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv = x$getinv()
        
        if (!is.null(inv)){
                message("getting cache data")
                return(inv)
        }
        
        data <- x$get()
        inv <- solve(data)
        x$setinv(inv)
        inv
}

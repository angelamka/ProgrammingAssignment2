## These functions create a matrix, cache its inverse and retrieve
## the cache if it has already been calculated. 

## Create a matrix and cache the matrix's inverse

makeCacheMatrix <- function(x = matrix()) {
        ## at first, set inverse to NULL and set matrix using y:
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        ## get the contents of the matrix:
        get <- function() x
        
        ## set the inverse of the matrix:
        setinv <- function(solve) inv <<- solve
        
        ## get the inverse of the matrix:
        getinv <- function() inv
        
        ## list all the results
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)        

}


## If the inverse of the matrix has been calculated and has not 
## changed, retrieve it from cache. Otherwise, calculate the 
## matrix's inverse.  

cacheSolve <- function(x, ...) {
        ## get the inverse from the output of makeCacheMatrix. 
        inv <- x$getinv()
        
        ## if the content of inverse is not NULL, get and 
        ## return cached data
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        ##if the content if NULL, get the matrix, calculate its
        ## inverse, set the inverse and display the result
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
        
}

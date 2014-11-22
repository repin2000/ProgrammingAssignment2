## Put comments here that give an overall description of what your
## functions do

## Tried a little OOP here...

makeCacheMatrix <- function(x = matrix()) {
    invPr <- NULL
    ## settting the matrix
    set <- function( matrix ) {
        m <<- matrix
        invPr <<- NULL
    }
    
    ## getting the matrix
    get <- function() {
        return(m)
    }
    
    ## setting the inverse 
    setInv <- function(inv) {
        invPr <<- inv
    }
    
    ## getting the inverse
    getInv <- function() {
        invPr
    }
    
    ## Return a list of the methods
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInv()
    
    ## Some error handling
    if( !is.null(m) ) {
        return(m)
    }
    data <- x$get()
    
    ## calculate & return
    m <- solve(data) %*% data
    x$setInv(m)
    return(m)
}

## The functions makeCacheMatrix and cacheSolve create and manipulate an object 
## that holds a matrix and its inverse. The matrix- and its inverse, if the
## inverse has been computed- are cached in an environment outside the current
## environment.

##
## Sample usage:
## > xx <- makeCacheMatrix(m) ## where m is a square numeric invertible matrix.
## > cacheSolve(xx)           ## calculate, store and return inverse matrix.
## > cacheSolve(xx)           ## retrieve and return inverse matrix.

##
## makeCacheMatrix takes a matrix as input, creates four methods, and returns a
## list of those methods. The methods support
##    -Storage of the input matrix and its inverse in an external environment
##    -Retrieval of the input matrix and its inverse from the external environment
makeCacheMatrix <- function(x = matrix()) {
    ## Initialize to NULL so caller of the getInverse method in will know
    ## xInverse has not yet been calculated
    xInverse <- NULL
    
    ## setMatrix stores a new input matrix in external environment and indicate
    ## to caller of getInverse method that inverse of the new input matrix has
    ## not yet been calculated
    setMatrix <- function(y) {

        x <<- y
        xInverse <<- NULL
    }
    ##
    
    ## 
    ## getMatrix retrieves previously-cached input matrix from external
    ## environment
    getMatrix <- function() x
    ##
    
    ## 
    ## setInverse stores inverse of the original input matrix in external
    ## environment. setInverse relies on the calling function for accuracy- it
    ## does not verify that invMatrix is the inverse of the original input
    ## matrix
    setInverse <- function(invMatrix) xInverse <<- invMatrix
    ##
    
    ##
    ## getInverse retrieves previously-cached input matrix from external environment
    getInverse <- function() xInverse
    ##
    
    ##
    ## Return a list of the methods we just defined
    list(setMatrix = setMatrix, getMatrix = getMatrix, 
         setInverse = setInverse, getInverse = getInverse)
} ## end of makeCacheMatrix


## 
## cacheSolve takes as input an object created by makeCacheMatrix. cacheSolve
## assumes that the 'makeCacheMatrix' object contains a square numeric
## invertible matrix.
## cacheSolve returns the inverse of that matrix. If this is the first call to
## cacheSolve, the inverse is stored in the 'makeCacheMatrix' object for future
## use.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    xInverse <- x$getInverse()
    if(!is.null(xInverse)) {
        ## Case I: Inverse of the original matrix has already been calculated
        ## and cached in an external environment, and we have just retrieved it-
        ## so return it and we're finished
        message("retrieving cached data")
        return(xInverse)
    }
    ## 
    ## Case II: Inverse of the original matrix has not yet been calculated. So
    ## retrieve the original matrix, invoke solve to calculate its inverse, and
    ## cache the result in external environment. Finally, return the inverse.
    data <- x$getMatrix()
    xInverse <- solve(data, ...)
    x$setInverse(xInverse)
    xInverse
} ## end of cacheSolve

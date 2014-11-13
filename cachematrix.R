## A functional derivate of the cacheVector example to cache inverted matrices.

makeCachedMatrix <- function(x = matrix) {
    
    ## When created there is no calculated inverted matrix
    im <- NULL
    
    ## a function to set a new matrix; the old inverted matrix will be cleared (=set to NULL).
    set <- function(y) {
        x <<- y
        im <<- NULL
    }
    
    ## a funtion to get the regular matrix
    get <- function() x    
    
    ## a function to cache the inverted matrix
    setinverted <- function(inverted) im <<- inverted   
    
    ## a funtion that returns inverted matrix (or NULL incase there's no inverted matrix calculed/set)
    getinverted <- function() im
    
    list(set = set, get = get, setinverted = setinverted, getinverted = getinverted)
    
}


cacheSolve <- function(x, ...) {
    
    ## Trying to get inverted matrix
    im <- x$getinverted()
    
    ## If the inverted matrix is not NULL, that is, there's already cached inverted matrix, function returns that matrix.
    if (!is.null(im)) {
        message("Fetching cached inverted matrix")
        return (im)
    }
    ## Otherwise the function calculates the inverted matrix...
    message("No cached inverted matrix, calculating...")
    
    temp <-x$get()      ## Get the original matrix
    
    im <- solve(temp, ...)      ## Solve the inverted matrix
    
    x$setinverted(im)       ## Cache the inverted matrix for future use.
    
    im      ## Return the inverted matrix
}

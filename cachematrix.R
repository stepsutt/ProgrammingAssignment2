## The function takes a matrix as a parameter and then creates a list
## of 4 functions to operate on that matrix
## set, get, setinverse and getinverse

makeCacheMatrix <- function(x = matrix()) {

    ## initialise internal variable
    m <- NULL
    ## create set function
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    ##create get function
    get <- function() x
    ## create setinverse storing matrix passed to it
    setinverse <- function(sol) m <<- sol
    ## create getinverse which returns what has been cached
    getinverse <- function() m
    ## return the list of functiond
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## Takes a makeCacheMatrix as input and retrieves the cached inverse
## if available otherwise calculates the inverse and cahces the 
##result

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    ##retrieve the cached inverse
    m <- x$getinverse()
    if(!is.null(m)) {
        ## if it is not null then a cached value exists
        message("getting cached data")
        ## return the cached value and exit
        return(m)
    }
    ## no cached value exists so get the matrix
    data <- x$get()
    ## now calculate the inverse
    m <- solve(data, ...)
    ## and cache it for later
    x$setinverse(m)
    ## return the inverse
    m
    
}

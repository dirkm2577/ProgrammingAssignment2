## The function 'makeCacheMatrix' takes a matrix and stores the inverse in  
## a global variable 'm' and provides the functions for 'cacheSolve' to get or set
## the inverse values.

## The function 'cacheSolve' returns the stored inverse matrix or calculates the
## inverse if the value was not stored already.


# creates matrix object and stores inverse values for access through 'cacheSolve' 
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL # 
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x # returns value of original matrix
    setinverse <- function(solve) {m <<- solve} # is called by 'cacheSolve' to
                                                # calculate and store inverse matrix
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## The cacheSolve function takes the matrix created by the makeCacheMatrix function
## and returns the inverse of the matrix from the cache or calculates an inverse of
## the matrix.

cacheSolve <- function(x, ...) {
    m <- x$getinverse() # access the global variable 'm' and get the stored value
    if(!is.null(m)) {   # if inverse was already cached, print message and return
                        # the value
        message("getting cached data")
        return(m)
    }
    data <- x$get()     # if 'm' had no stored value ('NULL') we get the original 
                        # matrix and calculate, store and return the inverse
    m <- solve(data, ...)
    x$setinverse(m)
    m
}

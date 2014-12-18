## The function 'makeCacheMatrix' takes a matrix and stores the inverse in  
## a global variable.
## The function 'cacheSolve' returns the stored inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    
    get <- function() {x}
    setsolve <- function(solve) {m <<- solve}
    getsolve <- function() {m}
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)

}


## The cacheSolve function takes the matrix created by the makeCacheMatrix function
## and returns the inverse of the matrix from the cache or creates an inverse of the matrix

cacheSolve <- function(x, ...) { ## Return a matrix that is the inverse of 'x'
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

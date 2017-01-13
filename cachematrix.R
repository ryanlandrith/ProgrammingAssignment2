
## makeCacheMatrix defines a get and set function for both caching the original matrix 
#and the inverse. It returns a list of the four functions and contains pointers to each 
#as well as the variables "inv" and "x"

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function()
        x
    setinv <- function(inverse)
        inv <<- inverse
    getinv <- function()
        inv
    list(
        set = set,
        get = get,
        setinv = setinv,
        getinv = getinv
    )
}


## Return a matrix that is the inverse of 'x'

cacheSolve <- function(z, ...) {
    ## z is an object of type "makeCacheMatrix" i.e. it is a list of functions and contains 
    #pointers all variables in that environment
    inv <- z$getinv()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- z$get()
    inv <-
        solve(data, ...)# This ... is passed from the function to specifiy solve options
    z$setinv(inv)
    inv
}

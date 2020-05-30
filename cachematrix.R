## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    mx <- NULL
    set <- function(y) {
        x <<- y
        mx <<- NULL
    }
    get <- function()
        x
    setmxi <- function(solve)
        mx  <<- solve
    getmxi <- function()
        mx
    list(
        set = set,
        get = get,
        setmxi = setmxi,
        getmxi = getmxi
    )
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    mx <- x$getmxi()
    if (!is.null(mx)) {
        message("getting cached data")
        return(mx)
    }
    data <- x$get()
    mx <- solve(data, ...)
    x$setmxi(mx)
    mx
}

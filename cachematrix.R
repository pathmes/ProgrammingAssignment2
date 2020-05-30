## The pair of functions 'makeCacheMatrix' and 'cacheSolve' together make 
## computing the inverse of a matrix efficient.  They cache the inverse of 
## a given matrix and return the cahced inverse matrix if the original matrix
## has remained unchanged


## 'makeCacheMatrix' - this function creates the template of a matrix to 
## cache the inverse matrix  produced by the 'cacheSolve' function 

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

## The list produced in the next section allows the objects to be used in the 
## next function and the elements are accessed by the $ sign
    
    list(
        set = set,
        get = get,
        setmxi = setmxi,
        getmxi = getmxi
    )
}


## 'cacheSolve' computes the inverse of the matrix if the matrix had changed
## using the solve() function.
## If the matrix has not changed the previously cached inverse matrix 
## is returned with the message "getting cached data"

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

## I basically modeled the behavior from the sample.

## As with the sample, makeCacheMatrix creates a list containing a function to
## set and the get the value of the matrix
## get and set the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}


## Again modeling from the sample, cacheSolve basically calculates the inverse
## using the built in solve() function.
## As with sample, it checks if inverse has already been calculated and returns that.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        ## introducing matrix id as the identity matrix of the parameter matrix
        id <- diag(nrow(data))
        inv <- solve(data, id, ...)
        x$setinv(inv)
        inv
}

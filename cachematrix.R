## These two functions create a list that stores a matrix and cache's
##its inverse. 

## makeCacheMatrix creates a list that sets the value of the matrix,
## gets the value of the matrix, sets the value of the inverse matrix,
## and gets the value of the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {

        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
        
}


## cachSolve calculates the inverse of the matrix created in the
## makeCacheMatrix function and sets the value of the matrix in the
## cache using the setinv function. If the inverse matrix has already
## been calculated, cachSolve gets the inverse matrix from the cache 
##and skips the computation.

cacheSolve <- function(x, ...) {
       
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m

}



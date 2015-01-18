## Cache the inverse of a matrix
## When the inverse is calculated, it is stored within the matrix object
## When available, the cached inverse is used instead of re-calculating
## PRECONDITION: matrix passed as input is invertible


## Takes a matrix as input and creates a matrix object with cached inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Calculates the inverse of a matrix created using makeCacheMatrix

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}

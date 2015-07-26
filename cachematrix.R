## makeCacheMatrix creates/returns functions used by cacheSolve 

makeCacheMatrix <- function(x = matrix()) {
        ## m will store the cached values, to create, initialzed NULL
        m <- NULL

        ## creates the matrix in the working environment
        set <- function(y) {
                x <<- y
                m <<- NULL
        }

        ## calculate and store the inverse of the matrix
        get <- function() x
        setinv <- function(inverse) m <<- inverse
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve calculates the inverse of the matrix created with makeCacheMatrix
## if the inverted matrix isn't cached, it is created and inverted value is cached

cacheSolve <- function(x, ...) {
        inv = x$getinv()
        
        # returns the inverse from the cache if already calculated
        if (!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        
        # else, calculates the inverse
        m = x$get()
        inv = solve(m, ...)
        
        # caches the value of the inverse matrix
        x$setinv(inv)
        
        return(inv)
}
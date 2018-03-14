## Set matrix value, get matrix value, set inverse value, get inverse value

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        print(list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse))
}


## Computes the inverse of the matrix or pulls from cache

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached matrix")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        return(m)
}

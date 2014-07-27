

## This fucntion creates a Matrix

makeCacheMatrix <- function(x=matrix()){
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m {
                list (set = set, get = get,
                      setinverse = setinverse, 
                      getinverse = getinverse)
        }
}


## This function gets the inversere of above  matrix but first checkes to see if the inverse has all ready been calculated.

cachesolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

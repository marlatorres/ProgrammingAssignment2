## The two functions are used to solve for the inverses of invertible matrices. 

## This function sets its inverse as null at first and caches it once set. 

makeCacheMatrix <- function(m = matrix()) {
        mt <- NULL
        set <- function(y) {
                m <<- y
                mt <<- null
        }
        get <- function() m
        setI <- function(inverse) mt <<- inverse
        getI <- function() mt
        list(set = set, get = get, setI = setI, getI = getI)
}

## This function tries to check if there is a cached inverse to the set matrix. If
## there is none yet, it goes back by getting the original matrix and then
## proceeds to getting its inverse

cacheSolve <- function(m, ...) {
        mt <- m$getI()
        if(!is.null(mt)) {
                message("Getting cached data...")
                return(mt)
        }
        data <- m$get()
        mt <- solve(data,...)
        m$setI(mt)
        mt
}

##End

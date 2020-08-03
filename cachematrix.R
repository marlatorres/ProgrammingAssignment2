## The two functions are used to solve for the inverses of invertible matrices. 

## This function sets its inverse as null at first and caches it once set. 

makeCacheMatrix <- function(m = matrix()) {
        mt <- NULL   #Sets the inverse matrix as null
        set <- function(y) {
                m <<- y     #Caches m
                mt <<- null   #Caches mt
        }
        get <- function() m     #gets the original matrix m
        setI <- function(inverse) mt <<- inverse    #function to set the inverse mt
        getI <- function() mt     #returns the inverse matrix
        list(set = set, get = get, setI = setI, getI = getI)
}

## This function tries to check if there is a cached inverse to the set matrix. If
## there is none yet, it goes back by getting the original matrix and then
## proceeds to getting its inverse

cacheSolve <- function(m, ...) {
        mt <- m$getI()          
        if(!is.null(mt)) {     #Checks if there is an inverse already
                message("Getting cached data...")
                return(mt)
        }
        data <- m$get()      #Gets the matrix m
        mt <- solve(data,...)   # Solves for the inverse of m
        m$setI(mt)     #Sets the inverse to mt
        mt             # Returns mt
}

##End

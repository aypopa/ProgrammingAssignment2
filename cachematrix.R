## Functions for a special matrix object capable of storing and maintaining 
## cache of its original and inverse.


## Define 4 basic IO functions:
## set - save (new) matrix (x) (erasing existing cache in process)
## get - read saved matrix (x)
## setinverse - cache result (inv)
## getinverse - read cached result (inv)

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL

        }
        get <- function() x
        setinverse <- function(z) inv <<- z
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Uses matrix in "makeCacheMatrix" format:
## 1. Checks if any previous inverse (inv) is cached 
## 2. If not, calculates new inverse and caches it

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached inverse")
                return(inv)
        }
        data <- x$get()
        z <- solve(data)
        x$setinverse(z)
        message("caching new inverse")
        z
}

#Define matrix:
#m <- matrix(c(2,3,1,4,1,2,1,1,1), nrow = 3, byrow=TRUE)
m <- matrix(c(1,3,1,4,1,2,1,1,1), nrow = 3, byrow=TRUE)

#Load matrix into special object:
mc <- makeCacheMatrix(m)

#Change matrix
mc$set(matrix(c(5,3,1,4,1,2,1,1,1), nrow = 3, byrow=TRUE))

#Read cache if exists, calculate it otherwise.
cacheSolve(mc)


## This function creates a special "matrix" object that can cache its inverse. Does this by creating a list containing a function to:
# set the value of the matrix
# get the value of the matrix 
# set the value of the inverse of the matrix
# get the value of the inverse of the matrix
makeCacheMatrix <- function(x = numeric()) {
        m <- NULL
        set <- function(y) { # set the value of the matrix
                x <<- y
                m <<- NULL
        }
        get <- function() x  # get the value of the matrix 
        setinverse <- function(inverse) m <<- inverse # set the value of the inverse of the matrix
        getinverse <- function() m                    # get the value of the inverse of the matrix
        # create special "matrix" object, i.e., list
        list(set = set, get = get,
             setinverse = setinverse, 
             getinverse = getinverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) { #if inverse of matrix exists in cache we will simply retrieve it
                message("getting cached data") #inform user matrix inverse was cached 
                return(m) #existing matrix inverse is retrieved
        }
        data <- x$get()
        m <- solve(data) #Computing the inverse of a square matrix is done via the solve function in R: solve(X) returns its inverse.
        x$setinverse(m) # calculated matrix inverse is returned
        m
}

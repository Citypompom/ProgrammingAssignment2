## The two functions below are used to return the invese of a matrix, they work in two steps:
## step 1: The "makeCacheMatrix" function will take a matrix as an input and create a list
## step 2: The "cacheSolve" function will then use the list created in step 1 as input and return the 
## inverse of the matrix which was used as the input to step 1



## The "makeCacheMatrix" function below creates a special "matrix" object that can be used for the 
## next function "cacheSolve" to cache its inverse.
## the input of this function is a matrix
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The "cacheSolve" function below computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated before, 
## then the cachesolve should retrieve the inverse from the cache.
## if it is called the first time, it will return inverse without the message "getting cached data"
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
        
}

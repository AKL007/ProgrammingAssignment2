## The makeCacheMatrix function creates a list of functions to set & get the
## value of the matrix and the value of the inverse of the matrix
## The cacheSolve function gets the inverse of a matrix from the cache if available
## else computes it and then updates the cache.

##This vector creates a list of functions to
##1) Set value of the matrix
##2) Get value of the matrix
##3) Set value of the inverse of the matrix
##4) Get value of the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        
        list(set = set, get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)
}


## This function first checks of the inverse of the matrix is stored in cache and
## fetches that, else it computes the inverse and sets the value of the inverse
## in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)){
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$setinverse(i)
        i
        
}

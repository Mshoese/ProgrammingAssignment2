## Below is a pair of functions that cache the inverse of a matrix.

## a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}


## This function computes the inverse of the special "matrix" 
#returned by makeCacheMatrix above. 
#it retrieves the inverse from the cache if:
#it has already been calculated and the matrix has not change

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data.")
                return(i)  ## Return a matrix that is the inverse of 'x'
        }
        data <- x$get()
        i <- solve(data)
        x$setinverse(i)
        i   ## Return a matrix that is the inverse of 'x'
       
}

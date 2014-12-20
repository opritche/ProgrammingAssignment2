## The following functions will cache the inverse of a matrix
## or generate the inverse of a matrix

## set the value of the matrix
## get the value of the matrix
## set the inverse of the matrix
## get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {            
            inver <- NULL
            set <- function(y) {
                        x <<- y
                        inver <<- NULL
            }
            get <- function() x
            setinverse <- function(solve) inver <<- solve
            getinverse <- function() inver
            list(set = set, get = get,
                 setinverse = setinverse,
                 getinverse = getinverse)
                
}


## returns the inverse of a matrix, after first checking to 
## see if the inverse has been calculated

cacheSolve <- function(x, ...) {
            inver <- x$getinverse()
            if(!is.null(inver)) {
                        message("getting cached data")
                        return(inver)
            }
            data <- x$get()
            inver <- solve(data, ...)
            x$setinverse(inver)
            inver
}

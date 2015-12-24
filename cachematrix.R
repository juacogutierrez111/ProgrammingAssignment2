

## Given an invertible matrix(x), the function returns its inverse.

makeCacheMatrix <- function(x = matrix()) {

        m <- NULL

	  ##set the value of the matrix.
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
	  ##get the value of the matrix.
        get <- function() x

	  ##set the inverese of the matrix in the 'chache'
        setInverse <- function(inverse) m <<- inverse

	  ##gtt the inverese of the matrix stored in the 'chache'
        getInverse <- function() m

        list(set = set, get = get,
             setInverse = setInverse ,
             getInverse = getInverse )

}


## return: inverse of the original matrix input to makeCacheMatrix()

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()

	  # Check if the inverse is already in the 'cache'
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)

	  # sets the value of the inverse in the cache via the setinverse function
        x$setinverse(m)

        return(m)
}

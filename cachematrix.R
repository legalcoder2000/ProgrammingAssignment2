## Functions on this page were based on the assignments for assignment two of R Programming
## Forked from https://github.com/rdpeng/ProgrammingAssignment2 on June 18, 2014 
## Both functions work with a matrix and provide a resulting inverse that is cached - if a cache is available
## These functions assume that the matrix is always invertible

## The function is based off of makeVector 
makeCacheMatrix <- function(x = matrix()) {

	m <- NULL
	// functions contained within the makeCacheMatrix function
	set <- function(y) {
			x <<- y
			m <<- NULL
	}
	get <- function() x
	setinverse <- function(inverseMatrix) m <<- inverseMatrix
	getinverse <- function() m
	// functions are returned as a list
	list(set = set, get = get,
		 setinverse = setinverse,
		 getinverse = getinverse)
}


## This function returns a matrix that is the inverse of x
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getinverse()
        # The cache exists, pull the data
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
                
        }
        data <- x$get()
        message("solving for inverse")
        m <- solve(data, ...)
        x$setinverse(m)
        m

}

## Use the following to test the functions, calling makeCacheMatrix to start
matrixSize = 2
x <- matrix(rnorm(matrixSize*matrixSize), matrixSize, matrixSize)

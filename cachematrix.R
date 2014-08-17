## Two functions that cache inverse matrix

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	n <- NULL
	## Method to set the matrix
	set <- function(matrix) {
		x <<- matrix
		n <<- NULL
	}
	## Method to get the matrix
	get <- function() {
		x
		}
	## Method to set inverse matrix
	setInverse <- function(inverse) {
		n <<- inverse
		}
	## Method to get inverse matrix
	getInverse <- function() {
		n
	}
	## List of methods
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## return inverse matrix of x
        m <- x$getInverse()
        ## return inverse if it is taken
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ## get matrix
        data <- x$get()
        ## calculate inverse
        m <- solve(data) %*% data
        ## set inverse
        x$setInverse(m)
        ## return
        m
}

## makeCacheMatrix is a function which creates a special "matrix"
## object that can cache its inverse.
## CacheSolve computes the inverse of the special "matrix" returned
## by makeCacheMatrix above. If the inverse has already been
## calculated, then the cacheSolve should retrieve the inverse
## from the cache.

## This function gets a matrix input, creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) inv <<-inverse
	getinverse <- function() inv
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}


## This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already
## been calculated (and the matrix has not changed), then the
## cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
	inv <- x$getinverse()
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data)
	x$setinverse(inv)
	inv
}

## Sample output
## > x<-rbind(c(1,2),c(3,4))
## > x
##      [,1] [,2]
## [1,]    1    2
## [2,]    3    4
## > matrix=makeCacheMatrix(x)
## > matrix$get()
##      [,1] [,2]
## [1,]    1    2
## [2,]    3    4
## > cacheSolve(matrix)
##      [,1] [,2]
## [1,] -2.0  1.0
## [2,]  1.5 -0.5
## > cacheSolve(matrix)
## getting cached data
##      [,1] [,2]
## [1,] -2.0  1.0
## [2,]  1.5 -0.5

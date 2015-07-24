## Make a matrix, calculate it's inverse, and cache it.

## Make a matrix

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y){
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setInverse <- function(inverse) m <<- inverse
	getInverse <- function() m
	list(get = get, set = set,
	     setInverse = setInverse,
	     getInverse = getInverse)
}


## Calculate a inverse of a matrix by , and cache it. if we calculate once, we can get it from the cache when we need next time.

cacheSolve <- function(x, ...) {
        m <- x$getInverse()
	if(!is.null(m)){
		message("getting cache inverse")
		return(m)
	}
	data <- x$get()
	inverse <- solve(data)
	x$setInverse(inverse) 
	inverse
}

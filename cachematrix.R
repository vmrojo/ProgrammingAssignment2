
## Return an object that caches the inverse of a matrix
makeCacheMatrix <- function(x = matrix()) {

	# x - a matrix
	inverse <- NULL
	
	set <- function(y) {
		x <<- y
		inverse <<- NULL
	}
	
	get <- function() {
		x
	}
	
	setinverse <- function(inv) {
		inverse <<- inv
	}
	
	getinverse <- function() {
		inverse
	}
	
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Return a matrix that is the inverse of 'x'. 
## The inverse is computed if a value was not cached or the matrix changed
cacheSolve <- function(x, ...) {
        
		inv <- x$getinverse()
		
		#No inverse in cache, calculate
		if(is.null(inv)) {
			#We assume matrix has an inverse
			inv <- solve(x$get())
			x$setinverse(inv)
		}
		
		inv
}

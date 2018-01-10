## The idea behind these functions is that you want to prevent time wasting situations. If you need to compute a time-consuming problemand already have the answer cached, then these functions will return the answer to you before having to repeat a potential repetitive calculation. This is a good example for loops, or when the contents of a vector are not changing and thus would waste time repeating the calculation when you already have the answer cached.

## The first function will make a special vector or "cacheMatrix" and calculate the mean. The "cachemean" portion checks if the answer is already computed and cached, and if it is not the function then computes it and returns the mean for the vector.

makeCacheMatrix <- function(x = numeric()) {
	inver <- NULL
	set <- function(y) {
		x <<- y
		inver <<- NULL
	}
	get <- function() x
	setInverse <- function(inverse) inver <<- inverse
	getInverse <- function() inver
	list(set = set, get = get,
		setInverse = setInverse,
		getInverse = getInverse)
}
cachemean <- function(x, ...) {
	inver <- x$getInverse()
	if(!is.null(inver)) {
		message("getting cached data")
		return(inver)
	}
	data <- x$get()
	inver <- Inverse(data, ...)
	x$setInverse(inver)
	inver
}

## The cacheSolve part of the function will return to the user the value for the inverse of x, or if that value has not been cached and computed already, it will first calculate before returning the value.

cacheSolve <- function(x, ...) {
      inver <- x$getInverse()
      if(!is.null(inver)) {
      		message("getting cached data")
      		return(inver)
      }
		mat <- x$get()
		inver <- solve(mat, ...)
		x$setInverse(inver)
		inver
}

## the x in cacheSolve has to be an ouput by makeCacheMatrix(
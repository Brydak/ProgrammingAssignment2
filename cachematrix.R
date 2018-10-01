## the functions allow to store a matrix, calculate its inversion, store 
## it in a "cache" and update/retrieve their values


## first function creates an object that stores a matrix and its inversion and 
## allows their updates and retrieval

makeCacheMatrix <- function(X = matrix()){

	I <- NULL

	set <- function(Y){
	X <<- Y
	I <<- NULL
	}	
	
	get <- function() X
	setinverse <- function(inverse) I <<- inverse
	getinverse <- function() I
	list(set = set, get = get, setinverse = setinverse,
	getinverse = getinverse)
}

## second function checks whether cache (an object created by makeCacheMatrix)
## contains inverse of a matrix, if yes retrieves it if not calculates inverse, 
##updates the cache and retrieves its value

cacheSolve <- function(x, ...){
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

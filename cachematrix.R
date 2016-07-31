
## Author :Bhavana V
## Date :30th June, 2016


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
  	set <- function(y) {
    		x <<- y  			## Assign the input argument to x object in parent environment
    		m <<- NULL 			##Assign the value NULL to the m object in the parent environment
  	}
  get <- function() x				## Retrieve the value of x from parent environment of makeCacheMatrix
  setinverse <- function(solve) m <<- solve	## Set the computed result of the inverse of the input matrix
  getinverse <- function() m			##Retrieve the vale of m
  list(set = set, 				## Gives the name set to the set() funtion defined above
	get = get,				## Gives the name get tot he get() function defined above
	setinverse = setinverse,		## Gives the name setinverse to the setinverse() funtion defined above
        getinverse = getinverse)		## Gives the name getinverse to the getinverse() function defined above
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (## and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
		m <- x$getinverse()		## Gets the inverse from cache
  		if(!is.null(m)) {		## Checks if the inverse is already cached
    			message("getting cached data")
    			return(m)		## function returns m - cached value and terminates 
  		}
  		data <- x$get()			## Fetches the matrix to compute inverse 
  		m <- solve(data, ...)		## HERE is where the MAGIC happens! - Computing the inverse  
  		x$setinverse(m)			## Stores the inverse in cache
  		m        			## Returns m - invered matrix and terminates 
	}

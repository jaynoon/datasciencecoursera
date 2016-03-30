#makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {  #set value of matrix
        x <<- y
        inv <<- NULL
    }
    get <- function() x  #get value of matrix
    setinverse <- function(inverse) inv <<- inverse  #set value of inverted matrix
    getinverse <- function() inv  #get value of inverted matrix
    list(set = set, get = get, 
	setinverse = setinverse, 
	getinverse = getinverse)
}

#cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {        # if iversion was already calculated, retrieve the inverse
        message("getting cached data.")
        return(inv)
    }
    dat <- x$get()
    inv <- solve(dat)
    x$setinverse(inv)
    inv
}
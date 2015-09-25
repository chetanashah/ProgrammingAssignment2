##makeCacheMatrix: This function creates a special "matrix" object that can cache ##its inverse.
##cacheSolve: This function computes the inverse of the special "matrix" returned ##by makeCacheMatrix above. If the inverse has already been calculated (and the ##matrix has not changed), then the cachesolve should retrieve the inverse from ##the cache.


## makeCacheMatrix - has 3 functions - 
## a)get() to retrieve the matrix
## b) set() to the inverse of the input matrix
## c) getInvMatrix() to retrieve the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
 	m <- NULL
        set <- function(y) {
                m <<- solve(y)
		m
                
        }
        get <- function() x

	getInvMatrix<- function() m
        
	list(set = set, get = get, getInvMatrix=getInvMatrix)
}


## cacheSolve Function - checks of the inverse of the matrix has been calculated 
## if computed before, it returns the cached inverse matrix
## if not then this function calculates the inverse of the matrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
 	m<-x$getInvMatrix() 

        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
	
        m <- solve(data, ...)
	 message("calculated in 'CacheSolve'function")
        return(m)

}

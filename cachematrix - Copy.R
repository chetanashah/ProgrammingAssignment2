## Put comments here that give an overall description of what your
## functions 

## makeCacheMatrix - has 2 functions - 
## a) set() to initialize a new matrix
## b) get() to retrieve the matrix
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


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
 	m<-x$getInvMatrix() 

        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
	
        m <- solve(data, ...)
	 message("calca data")
        return(m)

}


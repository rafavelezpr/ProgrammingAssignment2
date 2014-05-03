##This function, makeCacheMatrix, creates a special "matrix", which is really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        
        list (set = set, get = get, 
                setinverse = setinverse,
                getinverse = getinverse)
                
}


## The following function calculates the inverse of the special "matrix" created with the above function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return (m)
        }
                
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
        
}        

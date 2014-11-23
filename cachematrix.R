## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        
        # set should be used to alter the matrix
        # it invalidates the cache
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        # get simply returns the raw matrix
        get <- function() {
                x
        }
        
        # setinv sets the inv variable
        # should be used only by cacheSolve
        setinv <- function(i) {
                m <<- i
        }
        
        # getinv gets the cached inverse
        getinv <- function() {
                m
        }
        
        # return the special matrix
        list(set = set,
             get = get,
             setinv = setinv,
             getinv = getinv)    
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        # get the cached inverse
        m <- x$getinv()
        
        if(!is.null(m)) {
                # if the inverse if actually cached, just return it
                message("getting cached inverse")
                return(m)
        }
        
        # otherwise, calculate the inverse and cache it
        matr <- x$get()
        m <- solve(matr, ...)
        x$setinv(m)
        
        return(m)
}

## Example:
# mtr <- makeCacheMatrix(matrix(1:4, 2, 2))
# cacheSolve(mtr)
# cacheSolve(mtr)  ## "getting cached inverse"
# matr$set(matrix(5:8, 2, 2))
# cacheSolve(mtr)
# cacheSolve(mtr)  ## "getting cached inverse"


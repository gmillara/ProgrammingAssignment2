## These functions allow for the value of a matrix to be stored and 
## its inverse matrix to be computed and stored to (and retrieved from) cache. 

## The function makeCacheMatric() forms a 'matrix' class with additional storage 
## features that allow for its inverse value to be cached, along with 
## another function cacheSolve() which will compute, set and return the inverse of the 
## matrix if the inverse hasn't been previously computed, or to retrieve
## the value of the inverse from cache and return it. 


# The function makeCacheMatrix, returns a list of functions where
# each function element respectively performs the operations:

#    1.) set() -- sets the value of the matrix [ stores the matrix value
#        to cache,and defaults the cached value of the inverse to NULL ] 
#    2.) get() -- returns value of matrix
#    3.) setinv() -- sets the value of the inverse [ sets the value 
#        of the inverse of the matrix to cache ]
#    4.) getinv() -- returns the value of inverse

# A matrix class argument is optionally passed to initialize
# the stored matrix i.e the value returned by get()

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverseM){
                inv <<- inverseM
        }
        getinv <- function() inv
        
        list(set=set,get=get,setinv=setinv,getinv=getinv)
        
}


## The function cacheSolve() will return the matrix value of the inverse of the
## 'matrix' valued argument passed as x. The 'matrix' value passed is the list of 
## functions returned by the function makeCacheMatrix(). If the 'matrix' has a 
## previously cached value of the inverse of the matrix it will return the cached
## value. If no cached value exists, the inverse if the matrix is computed, 
## stored to cache and returned.

cacheSolve <- function(x, ...) {
        
        inv <- x$getinv()
        if (!is.null(inv)){
                message("getting cached inverse")
                return(inv)
        }
        matrix <- x$get()
        inv <- solve(matrix, ...)
        x$setinv(inv)
        inv  ## Returns a matrix that is the inverse of 'x'
}

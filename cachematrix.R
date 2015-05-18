## This document contains 2 functions: makeCacheMatrix and cacheSolve
## These functions cache the inverse of a matrix


## This function creates a special "matrix" object that can cache its inverse
## makeCacheMatrix stores a list of 4 functions

makeCacheMatrix <- function(x = matrix()) {
        #First we set the inverse of the matrix, i, to null
        i<-NULL
        
        #The first function sets the matrix and sets it's inverse to null 
        set<-function(y) {
                x<<-y
                i<<-NULL
        }
        #The second function returns the currently set matrix
        get<-function() x
        #The third function sets the inverse of a matrix (i), given it's value
        setinv<- function(inv) i<<- inv
        #The fourth function returns the inverse of a matrix (i)
        getinv<- function () i
        #Finally, a list containing these functions is returned
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix not changed), the cachesholve retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Returns a matrix that is the inverse of 'x'
        ## The input x should be a special matrix created with makeCacheMatrix
        i <- x$getinv()
        # Checks if the value i has been stored previously and returns it if so
        if (!is.null(i)) {
                message("getting cached inverse")
                return(i)
        }
        
        #If i had not ben cached, data gets the matrix stored with makeCacheMatrix
        data <- x$get()
        #i calculates the inverse of the matrix
        i <- solve(data)
        #now we cache the inverse
        x$setinv(i)
        #and finally return the inverse
        i
}
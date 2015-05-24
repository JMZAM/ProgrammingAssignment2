## This document contains 2 functions: makeCacheMatrix and cacheSolve
##      These functions cache the inverse of a matrix and use it 
##      if it has been cached instead of calculating it again


## 1. makeCacheMatrix: This function creates a special "matrix" object that can 
##      cache its inverse. It stores a list of 4 functions

makeCacheMatrix <- function(x = matrix()) {
        #First we set the inverse of the matrix, i, to null
        i<-NULL
        
        #The first function sets the matrix to the input and it's inverse to null 
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


## 2. cacheSolve computes the inverse of the special "matrix" returned by  
##      makeCacheMatrix above. If the inverse has already been calculated 
##      (and the matrix not changed), then cacheSolve retrieves the inverse 
##      from the cache.

cacheSolve <- function(x, ...) {
        ## Returns a matrix that is the inverse of 'x'
        ## The input x should be a special matrix created with makeCacheMatrix
        
        # First we retrieve the value of the inverse stored in the special matrix
        #      and store it in a variable (i)
        i <- x$getinv()
        
        # Then we check if the value (i) had been stored previously and return 
        #      it if so
        if (!is.null(i)) {
                message("getting cached inverse")
                return(i)
        }
        
        #If i had not been cached, data gets the matrix stored with makeCacheMatrix
        data <- x$get()
        #i calculates the inverse of the matrix
        i <- solve(data)
        #now we cache the inverse (i)
        x$setinv(i)
        #and finally return the inverse (i)
        i
}
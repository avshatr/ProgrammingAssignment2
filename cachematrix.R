## These functions create and manage a special object (a list of functions)
## that can store a numerical matrix, calculate its inverse and store the inverse in
## memory. The pre-calculated inverse matrix can be obtained from memory 
## instead of being calculated every time there is need to use it.


## The makeCacheMatrix function creates a special "matrix" object
## that can store a numerical matrix and its inverse (given the inverse as input)

makeCacheMatrix <- function(x = matrix()) {

inv <- NULL ##NULL value indicates that the inverse
## has not yet been computed 


set <- function(y) {

## 'x' and 'inv' already exist in the environment associated with makeCacheMatrix 

x <<- y ## assign the "contents" of the "matrix" with a given matrix y

inv <<- NULL ## NULL indicates that the object has been changed
## and the inverse should be recalculated

}



get <- function() return(x) ## return the current numerical matrix

setinv <- function(invmatr) inv <<- invmatr ## assign the inverse with a given
## matrix

getinv <- function() return(inv) ## return the current inverse of the matrix

list(set = set, get = get,
setinv = setinv,
getinv = getinv) ##return list of functions

}


## The cacheSolve function computes the inverse of the matrix stored
## in the object returned by makeCacheMatrix function. In case there have been no 
## changes in entries of the matrix since last time the inverse was calculated,
## the cached value of the inverse is returned.

cacheSolve <- function(x, ...) {
    
inv <- x$getinv() ## get the inverse stored in the object 'x'

if(!is.null(inv)) {
    
    message("getting cached data") ## the inverse is NULL only in two cases: 1) it has never
    ## been calculated and 2) the contents of the "matrix" have been
    ## changed and there is necessity to reassign the 'inv' variable in the environment
    ## associated with'x'. Thus, inv!=NULL means the cached inverse can be used safely.
    
    return(inv)
    }
    
    ## The following lines are reached only in case the inverse (variable 'inv' in the environment
    ## associated with 'x') of the matrix stored in 'x'  has to be assigned
    ## with the inverse of the matrix  for the first time or reassigned with a proper value due
    ## to changes in the matrix stored in 'x'
    
    xcontents <- x$get() ## retrieve the current contents of the "matrix" 'x'
    
    ## Assume the matrix stored in 'x' is square and its inverse exists
    inv <- solve(xcontents, ...) ## calculate the inverse 
        

x$setinv(inv) ## assign the 'inv' variable in the environment associated with 'x'
inv ## return the new value for the inverse
}

## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## function makeCacheMatrix creates a matrix containting
##  a function which job is to:
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }

    get <- function() x

    set_inverse <- function(inv_input) inv <<- inv_input
    get_inverse <- function() inv
    list(set = set, get = get,
         set_inverse = set_inverse,
         get_inverse = get_inverse)

}


## Write a short comment describing this function
## Next function calculates the inverse of the matrix 
## created with the makeCacheMatrix function. 
## It first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the 
## computation. 0therwise, it calculates the inverse of the 
## matrix and sets the value of the inverse in the cache via 
## the setinv function.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
inv <- x$get_inverse()
if(!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
}
data <- x$get()
inv <- solve(data, ...)
x$set_inverse(inv)
inv
        
}

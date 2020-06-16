## Put comments here that give an overall description of what your
## functions do

## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than computing it
## repeatedly (there are also alternatives to matrix inversion that we will
## not discuss here). Your assignment is to write a pair of functions that
## cache the inverse of a matrix.

## Write a short comment describing this function

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        a <- NULL
        set <- function(y) {
                x <<- y
                a <<- matrix()
        }
        get <- function() x
        setinv <- function(inverse) {
                a <<- inverse
        }
        getinv <- function() a
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## Write a short comment describing this function

## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then 
## `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        a <- x$getinv()
        if(!is.null(a)) {
                message("Getting cached data...")
                return(a)
        }
        data1 <- x$get()
        a <- solve(data1, ...)
        x$setinv(a)
        a ## Return a matrix that is the inverse of 'x'
}

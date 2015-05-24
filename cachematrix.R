## makeCacheMatrix & cacheSolve functions help us compute the inverse
## of a matrix and store it in cache for later use.

## When the user provides the necessary input, makeCacheMatrix function 
## sets up a "special" matrix object that can cache its inverse.
 
makeCacheMatrix <- function(x = matrix()) {
        
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        get <- function() x
        setInverse <- function(solve) m <<- solve
        getInverse <- function() m
        
        list(set=set, get=get,
             setInverse=setInverse,
             getInverse=getInverse)
}

## CacheSolve function computes the inverse of a matrix which comes from the
## makeCacheMatrix function. If we already have the inverse of a matrix 
## calculated and the input matrix did not change we should receive a "getting cached data"
## message and cacheSolve will load the inverse stored in the cache. 

cacheSolve <- function(x = matrix(), ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getInverse()
        
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        matrix <- x$get()
        m <- solve(matrix, ...)
        x$setInverse(m)
        print(m)
}

a <- makeCacheMatrix()
a$set(matrix(1:4, 2, 2))
cacheSolve(a)

## Both functions were written with the help from DanieleP:
## 'PA2-clarifying_instructions' at https://github.com/DanieleP/PA2-clarifying_instructions
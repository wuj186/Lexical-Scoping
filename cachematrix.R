# 1) create a matrix as input x
# 2) set the solved value "a" as a null
# 3) changed every reference to "solve" from "mean"


makeCacheMatrix <- function(x = matrix(rnorm(25, mean = 0, sd = 1),5,5)) {
        a <- NULL
        set <- function(y) {
                x <<- y
                a <<- NULL
        }
        get <- function() x
        seta <- function(solve) a <<- solve
        geta <- function() a
        list(set = set, get = get,
             seta = seta,
             geta = geta)
}



## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache. Solve function in R to Computing the inverse of a square matrix can be done with the.
## For example, if X is a square invertible matrix, then solve(X) returns its inverse.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        a <- x$geta()
        if(!is.null(a)) {
                message("getting inversed matrix from cache ")
                return(a)
        }
        data <- x$get()
        a <- solve(data, ...)
        x$seta(a)
        a
}

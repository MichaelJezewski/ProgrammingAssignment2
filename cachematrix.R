makeCacheMatrix <- function(x = matrix()) {      
## For project assignment 2.
## Its a matrix that does inverts that returns a list as the input to cacheSolve()
        inv = NULL
        set = function(y) {
                x <<- y
                inv <<- NULL
        }
        get = function() x
        setinv = function(inverse) inv <<- inverse 
        getinv = function() inv
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}

cacheSolve <- function(x, ...) {
## This gives the final output from makeCacheMatrix() function
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix
        inv = x$getinv()
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache
        if (!is.null(inv)){
                # get it from the cache and skips the computation. 
                message("getting cached data")
                return(inv)
        }
## Calucate the inverse if not already done
        mat.data = x$get()
        inv = solve(mat.data, ...)
        x$setinv(inv)
        return(inv)
}

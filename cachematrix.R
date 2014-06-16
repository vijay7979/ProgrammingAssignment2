## This program calculates the inverse of a given invertible matrix.
## Once the inverse has beeen calculated, it will be cached.
## The next time the inverse for the same matrix is called, it will 
## taken from the cache instead of being recalculated.

## "makecacheMatrix" allows a matrix to be inputted for its inverse to
## be evaluated through the "set function". We can get the inputted matrix
## through the "get" function.

makeCacheMatrix <- function(x = matrix()) {
        m = NULL
        set = function(y) {
               
                x <<- y
                m <<- NULL
        }
        get = function() x
        setinverse = function(inverse) m <<- inverse
        getinverse = function () m
        list(set=set, get=get,
             getinverse=getinverse,
             setinverse=setinverse)

}


## "cacheSolve" calculates the inverse of a given inverteble matrix, caches it
## and outputs the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m = x$getinverse()
        if (!is.null(m)) {
                message("getting cached data")
                return (m)
        }
        data = x$get()
        m = solve(data, ...)
        x$setinverse(m)
        m
}

## Example Instructions.
## We want to find the inverse of a 2 by 2 matrix as follows :
##     [1] [2]
## [1]  1   2
## [2]  3   4
##
## After running the functions :
## v = makeCacheMatrix() 
## v$set(matrix(1:4, 2, 2))
## v$get()
## cacheSolve(v)
## cacheSolve(v)
##
## The second time cacheSolve is run, you should see the following message :
## "getting cached data"












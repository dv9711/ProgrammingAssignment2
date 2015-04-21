## Assignment 2: caching the inverse of a matrix
## Part1: function creates a special matrix object
makeMatrix <- function(x = numeric()) 
{
    mat <- NULL
    set <- function(y) 
    {
        x <<- y
        mat <<- NULL
    }
    
    get <- function() x
    setinv <- function(solve) mat <<- solve
    getinv <- function() mat
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## Part2: function creates inverse of the matrix with solve() or from the cache
cacheInv <- function(x, ...) 
{
    mat <- x$getinv()
    if(!is.null(mat)) 
    {
        ## return of inverse matrix from cache
        message("getting cached data")
        return(mat)
    }
    ## calculation and return of inverse matrix
    data <- x$get()
    mat <- solve(data, ...)
    x$setinv(mat)
    mat
}

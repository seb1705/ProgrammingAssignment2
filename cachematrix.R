## makeCacheMatrix 
##   This function creates a special "matrix" object that can cache its inverse.
## cacheSolve
##   It computes the inverse of the special "matrix" returned by makeCacheMatrix 
##   If the inverse has already been calculated (and the matrix has not changed), 
##   then the cachesolve should retrieve the inverse from the cache.


## makeCacheMatrix is a function which creates special matrix object(x) 
##   which cache its inverse (inv). 
## Object returned by this function will have functions set, get, setsolve & getsolve  
##   set       store set new values to matrix 
##   get       retrieve values stored matrix values
##   setsolve  store inverse of the matrix in cache (inv)
##   getsolve  retrieve cached inverse  


makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) inv <<- solve
    getsolve <- function() inv
    list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## cacheSolve function takes special matrix object as input and output its inverse
##   it checks if  inverse of matrix is cached and returns from cache when present
##   when inverse is not cached, it calculates inverse and store in cache for next time


cacheSolve <- function(x, ...) {
    ## Check whether inverse of matrix is already cached, if yes return from cache
    i <- x$getsolve()
    if(!is.null(i)){
        message("getting cached data")
        return(i)
    }
    ## get the values stored in matrix x and calculate inverse using solve fun
    message("calculating inverse")
    m <- x$get()
    i <- solve(m, ...)
    ## Update cache with inverse and return calculated inverse
    x$setsolve(i)
    i
}

## This program is composed by 2 functions: makeCacheMatrix and cacheSolve. 
## Once executed, the first one allows to create a matrix and store it and its 
## inverse in the cache. The second one, returns the inverse of that matrix
## after having checked if it was in the cache to save calculation time.   


## makeCacheMatrix must be executed without any argument, after which returns
## a list composed by 4 functions: 

    ## 1 - When called allows to create a square matrix in cache given 
    ##     by the argument. It also restores the value of the inverse to NULL
    ## 2 - When called shows the value of the matrix stored in cache
    ## 3 - When called sets the given argument as the inverse of the matrix in
    ##     cache
    ## 4 - When called shows the value of the inverse stored in cache

makeCacheMatrix <- function(x = matrix()) {
    
    inv <- NULL
    setmatrix <- function (y){
       
        as.matrix(y)                ## Coherce y as matrix
        x <<- y                     ## Save "y" in the cache
        inv <<- NULL                ## Restore the value of "inv"
    
    }
    getmatrix <- function () x
    setinv <- function (inverse){
        
        inv <<- inverse             ##Set the value of the inverse in the cache
        
    }
    getinv <- function () inv
    list(setmatrix=setmatrix, getmatrix=getmatrix, setinv=setinv, getinv=getinv)
    
}


## cacheSolve needs as argument the first element of 'makeCacheMatrix' function.
## It checks wether the inverse of the matrix is stored in cache and in that 
## case returns its value. Otherwise, it calculates its and sets it in the cache.

cacheSolve <- function(x, ...) {
        
    inv <- x$getinv()               
    if(!is.null(inv)){              ##Check wether the inverse is in cache
        
        message("Getting cached data")
        return (inv)
    
    }
    matrix <- x$getmatrix()         ##Charge de value of the matrix in cache
    inv <- solve(matrix)            ##Calculate de inverse of the matrix
    x$setinv (inv)                  ##Set the new value of the inverse in cache
    inv
    
}

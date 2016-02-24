## This function is calculating the inverse of a matrix and saving it in a the 
##cache in in order to be used when its needed istead of being calculted many times
##specially when using the value in the loop which makes it very expencive.


## This function (first function) creates a special "matrix" object, which is really a list 
## containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {     #create a matrix object

        inv <- NULL                             # initialize an inv cache
        set <- function(y) {
                x <<- y                         ## assign the input matrix y to 
                                                ##to x from the parent Env.
                inv <<- NULL                    ## re-initialize m in the Function Env.
        }
        
        get <- function() x                                     ## get the matrix x
        setinverse <- function(inverse) inv <<- inverse         ## set the cache equal
                                                                ## to the inverse of the matrix x
        getinverse <- function() inv                              ## return the cached inverse of x
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


##This function is calculating the inverse of the special matrix that created in
##by the function makeCacheMatrix. Before calculating the inverse, it checks if 
##it has been already calculated, it doesn't calculate otherwise it calculates the inverse
## and set it to the cache by setinverse function.

cacheSolve <- function(x, ...) {
        
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}



## This function createst a list of functions to cache the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y){
                x <<- y
                i <<- NULL
        }
        get <- function(){
                x
        }
        setinverse <- function(inverse){
                i <<- inverse
        }
        getinverse <- function(){
                i
        }
        list(set = set, get = get, setinverse = setinverse, 
             getinverse = getinverse)
}


## This function tries to assign to i a cached inverse matrix, if it is null
## meaning there is nothing saved, it will calculate the inverse using
## solve(matrix)%%solve where %% is matrix multiplacation.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)){
                message("getting chached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data)%%data
        x$setinverse(i)
        i
}

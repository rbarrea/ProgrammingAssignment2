## makeCacheMatrix creates a special matrix, which is a list
##containing a function to do the following:
## 1) set the value of the matrix
## 2) get the value of the matrix
## 3) set the value of the inverse of the matrix
## 4) get the value of the inverse of the matrix


makeCacheMatrix <- function(MyMatrix = matrix()) {
        MyInverse <- NULL
        set <- function(y) {
                MyMatrix <<- y
                MyInverse <<- NULL
        }
        get <- function() MyMatrix
        setinverse <- function(MyMatrix) MyInverse <<- MyMatrix
        getinverse <- function() MyInverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## cacheSolve calculates the inverse of a square matrix
## if the inverse has been calcualtes already 
## and the matrix has not changed
## then the function retrieves the cached inverse function

cacheSolve <- function(x, ...) {
        MatrixInverse <- x$getinverse()
           if(!is.null(MatrixInverse)) {
                        message("getting cached data")
                        return(MatrixInverse)
           }
        
        Matrix2beInverted <- x$get()
        MatrixInverse <- solve(Matrix2beInverted)
        x$setinverse(MatrixInverse)
        MatrixInverse
        
        
}

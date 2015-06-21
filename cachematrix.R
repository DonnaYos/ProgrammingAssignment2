## These functions create a matrix object that stores the inverse
## Use makeCacheMatrix() to make the matrix object and cacheSolve(matrix object) to get the inverse
## This function is an object that stores a matrix and it's inverse so it does not have to be recomputed
## if the matrix changes, the inverse is recomputed and stored

makeCacheMatrix <- function(x = matrix()) {
        dims<-dim(x)
        inv<-matrix(data=NA,nrow=dims[1],ncol=dims[2])##sets the inverse to a matrix of NA's
        set <- function(y) {##this function resets the stored matrix and inverse in the case of a new input matrix
                x <<- y
                dims<-dim(x)
                inv<-matrix(data=NA,nrow=dims[1],ncol=dims[2])
        }
        get <- function() x
        setinverse <- function(sol) inv <<- sol
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## this function checks to see if the matrix inverse was computed, and if not, computes it

cacheSolve <- function(x) {
        ##input is the object created by makeCacheMatrix()
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.na(inv[1])) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
      
          x$setinverse(inv)
        inv
}

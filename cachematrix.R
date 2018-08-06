## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
      i <- NULL  ##initializes inverse property
  
      set <- function(matrix){  ##sets matrix
             m <<- matrix
             i <<- NULL
  }
  
      get <- function(){    ##gets matrix
             m
  }
  
       setinverse <- function(inverse){  ##sets matrix inverse
             i <<- inverse
  }    
  
       getinverse <- function(){  ##gets matric inverse
             i
  }
  
       list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  ##gives list
}



## Write a short comment describing this function

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    
        m <- x$getinverse()  ## Return a matrix that is the inverse of 'x'
        
        if(!is.null(m)){  ## Return inverse if set already
              message("retrieving the cached data")
              return(m)
        }
        
        obj <- x$get() ## Get matrix from object
        
        m <- solve(obj) %*% data ## Caculates inverse with matrix multiplication
        
        x$setinverse(m) ## Sets inverse of object
        
        m 
}

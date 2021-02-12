## Functions makeCacheMatrix and casheSolve: Cashing the inverse of a matrix

## Function discription: Creates a matrix that can cache the inverse

makeCacheMatrix <- function(x = matrix()) {
   iv <- NULL
   set <- function(y){
     x <<- y
     iv <<- NULL
   }
   get <- function() x
   setinverse <- function(solve)iv <<- solve
   getinverse <- function() iv
   list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse) 
}


## Function description: Computes inverse of a matrix, but only if inverse has
##                       not been calculated yet. Otherwise, it is retrieved 
##                       from the cashe. 

cacheSolve <- function(x, ...) {
  iv <- x$getinverse()
  if(!is.null(iv))  {
    message("getting cached data")
    return(iv)
  }
  else{ 
    data <- x$get()
    iv <- solve(data, ...)
    x$setinverse(iv)
    return(iv)
  }
}

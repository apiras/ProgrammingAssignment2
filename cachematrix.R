# Peer-graded Assignment: Programming Assignment 2: Lexical Scoping 

## angelo.piras@gmail.com

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y #store the matrix
    m <<- NULL # store the inverse
  }
  get <- function() x #return the matrix 
  setinverse <- function(inverse) m <<- inverse #set the value of inverse on m
  getinverse <- function() m #return the m value cached
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse() #call getinverse 
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...) #calculate the inverse of squared matrix
  x$setinverse(m)
  m
}

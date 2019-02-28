## After reading a lot of stuff online, I think replacing
## mean with inverse & numeric with matrix from the example 
## should do the trick

## This is the exact makeVector() example but:
## m is inv
## mean is inverse
## numeric is matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL 
  ## here a matrix is accepted as input and a second variable
  ## is created, called y
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  ## The x variable a hierarchy level higher is set equal to the newly created y
  ## The newly created inv is nullified (in case it was already in cache)
   
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  ##
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
## Again, this is the exact cacheMean() example but:
## m is inv
## mean is inverse
## numeric is matrix
## the mean() function is replaced by the solve() function as instructed

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
    ##this only happens if the inv variable is null and not set in the previous function makeCacheMatrix()
  }
  data <- x$get()
  inv <- solve(data)
  ## This was the mean() function in the example, now solve()
  x$setinverse(inv)
  inv
        ## Return a matrix that is the inverse of 'x'
}

## Sample run:
##
##> testMatrix <- rbind(c(1,2),c(2,1))
##> testCacheMatrix <- makeCacheMatrix(testMatrix)
##> testCacheMatrix$get()
##[,1] [,2]
##[1,]    1    2
##[2,]    2    1
##> cacheSolve(testCacheMatrix)
##[,1]       [,2]
##[1,] -0.3333333  0.6666667
##[2,]  0.6666667 -0.3333333
##> cacheSolve(testCacheMatrix)
##getting cached data. 
##[,1]       [,2]
##[1,] -0.3333333  0.6666667
##[2,]  0.6666667 -0.3333333

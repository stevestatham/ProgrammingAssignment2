## This script contains 3 functions:
## 1. makeCacheMatrix caches an input matrix
## 2. cacheSolve calculates the inverse of the matrix (solve) and saves it (setinv)
##    if not already solved.
##    If solved previously, it retrieves the previous solution (getinv)
## 3. test

## makeCacheMatrix function
## input x: a square invertible matrix
## return: a list containing the following functions:
##  1. set - set the matrix (nulls the inverse variable)
##  2. get - get the matrix
##  3. setinv - set the inverse
##  4. getinv - get the inverse
## return a list, used as the input to cacheSolve(), and containing those functions.

makeCacheMatrix <- function(x = matrix()) {

  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) i <<- inverse 
  getinv <- function() i
  list(set=set, get=get, 
       setinv=setinv, 
       getinv=getinv)
}

## cacheSolve function
## Return a matrix that is the inverse of 'x'
## x: input from makeCacheMatrix()
## return: inverse of the original matrix passed into to makeCacheMatrix()

cacheSolve <- function(x, ...) {
  
  i = x$getinv()
  
  if (!is.null(i)){ 
    message("getting cached data")
    return(i)
  }
  
  data = x$get()
  i = solve(data, ...)
  x$setinv(i)
  return(i)

}

## test function
## Call makeCacheMatrix with a square matrix (default is 1000 X 1000) as the input
##   and return a list containing a pointer to the input matrix and its functions
## Save the initial system time
## Run cacheSolve with the list returned from makeCacheMatrix.
##   Since this is the first run it will do the calculations.
## Print the time elapsed (displayed as NOT CACHED delta:)
## Save the system time again
## Run cacheSolve with the list returned from makeCacheMatrix.
##   Since this is the second run it will return the cached solution.
## Print the time elapsed (displayed as CACHED delta:)

test = function(nr=1000000, nrow=1000, ncol=1000){

  tester = makeCacheMatrix(matrix(rnorm(nr), nrow, ncol))
  
  s1 <- Sys.time()                           #Start Time  
  cacheSolve(tester)                         #inverse the matrix (not cached)
  dur <- Sys.time() - s1                     #Calculate the duration of the runtime
  print(paste('NOT CACHED delta: ', dur))    #Print runtime duration NOT CACHED

  s2 <- Sys.time()                           #Start Time
  cacheSolve(tester)                         #inverse the matrix (cached)
  dur <- Sys.time() - s2                     #Calculate the duration of the runtime
  print(paste('CACHED delta: ', dur))        #Print runtime duration CACHED

}

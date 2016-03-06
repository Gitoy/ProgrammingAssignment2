## The functions provide the inverse matrix of the matrix set.
## This is optimised for retrieving the inverse when the matrix 
## is not changed, by caching both the input matrix and its inverse.
## 
## usage is for example:
## i <- makeCacheMatrix()    # turn the variable in a special "matrix"
## i$set(matrix(1:4, 2, 2))  # fill the special "matrix" with a normal matrix
## cachemean(i)              # calculate the mean, if it's not already calculated



## This function sets a variable to work as a cache
## for the input matrix and potentially its inverse.

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y){
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolved <- function(solve) s <<- solve
  getsolved <- function() s
  list(set = set, get = get, setsolved = setsolved, getsolved = getsolved)
}


## This function returns the inverse matrix based on a special input "matrix".
## It tries to get the calculated inverse created by makeCacheMatrix. 
## If its not yet calculated it calculates the inverse matrix 
## from the data in makeCacheMatrix and caches it.

cacheSolve <- function(x) {
  ## Return a matrix that is the inverse of 'x'
  s <- x$getsolved()
  if(!is.null(s)){
    message("getting cached data")
    return(s)
  }
  
  data <- x$get()
  s <- solve(data)
  x$setsolved(s)
  s
}
## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
#pre-set a NULL value
  m <- NULL
#set a new matrix
  set <- function(y){
    x <<- y
    m <<- NULL
  }
#get data from an existing matrix
  get <- function() x
#solve the inverse of a matrix and cache it
  setSolve <- function(inverse) m <<- inverse
#get the inverse of a matrix
  getSolve <- function() m
  list(set = set, get = get, setSolve = setSolve, getSolve = getSolve)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
#check whether the matrix has been calculated inverse or not
  m <- x$getSolve()
  if (!is.null(m)){
    message("getting cached data")
    return(m)
  }
#if there is no inverse calculated before, then we calculate the inverse and cache it
  data <- x$get()
  m <- Solve(data)
  x$setSolve(m)
  m
  ## Return a matrix that is the inverse of 'x'
}

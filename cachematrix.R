## This function creates a matrix object that can cache its inverse.
## Each object that is created using this function has 4 functions. get(), set(), getmatrix() and setmatrix()


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(solve) m <<- solve
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


## This function checks if an inverse of a matrix already exists by accessing the other environment 
## where previous function was created (aka Lexical Scoping). If it is present there, it simply
## fetches that value and doesnt recompute. If its not present, then it computes an inverse of a matrix


cacheSolve <- function(x, ...) {
  m<-x$getmatrix()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m
}


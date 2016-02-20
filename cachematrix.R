## This function create a list of functions whcih can be called by a different function.
## The first element of the list is a function to set an object in an environment.
## The second elemente of the list is a function to get data.
## The third element of the list is to set the object in the outside environment to the value calculated in the function environment;
## The fourth element of the list is to get an object from the cache.. 

## Write a short comment describing this function
#~

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  set_ivmatrix <- function(inv_mat) m <<- inv_mat
  get_ivmatrix <- function() m
  list(set = set, get = get,
       set_ivmatrix = set_ivmatrix,
       get_ivmatrix = get_ivmatrix)
}


## Write a short comment describing this function
## This fucntion can call the list of the functions avaliable in the function makeCacheMatrix.
## It first check m to see if an object is stored in cache. If exit, it get out of the function.
## If the object is not in the list, then it call the 2nd function from makeCacheMatrix to calculate the inverse matrix, and 
## use the third function in makeCacheMatrix to set the calculated inverse matrix to the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$get_ivmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$set_ivmatrix(m)
  m
}

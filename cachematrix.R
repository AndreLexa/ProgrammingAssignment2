## This is the Programming Assignment 2: Lexical Scoping of the course "R Programming" at Coursera


## This function store a list of functions for a squared matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  # if not a squared matrix, get out
  if(ncol(x) != nrow(x)){
    print("Error, you need a squared matrix!")
    return(x)
  }
  
  # return the matrix
  get <- function() x
  
  # set the matrix
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  
  # return the stored inverse
  get_inv <- function() inv
  
  # set and store the inverse of the matrix
  set_inv <- function(i){
    inv <<- i
  }
  
  # stores the three functions
  list(get = get, set = set, get_inv = get_inv, set_inv = set_inv)
}


## This function calculate the inverted matrix, if necessary, and give it at it ends
cacheSolve <- function(x = makeCacheMatrix()) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$get_inv()
  
  if(!is.null(inv)){
    message("Getting cached inverted matrix:")
    return(inv)
  }
  
  message("Calculated inverted matrix:")
  inv <- solve(x$get())
  x$set_inv(inv) 
  
  inv
}

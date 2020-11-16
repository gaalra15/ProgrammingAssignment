makeCacheMatrix <- function( m = matrix() ) {
  
  # inverse
  i <- NULL
  
  #set the matrix
  set <- function( matrix ) {
    m <<- matrix
    i <<- NULL
  }
  
  # get the matrix
  get <- function() {
    
    m
  }
  
  #set the inverse of the matrix
  setInverse <- function(inverse) {
    i <<- inverse
  }
  
  #get the inverse of the matrix
  getInverse <- function() {
    ## Return the inverse property
    i
  }
  
  ## Return a list of the methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}



cacheSolve <- function(x, ...) {
  
  m <- x$getInverse()
  
  
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  
  # Get the matrix from our object
  data <- x$get()
  
  # Calculate the inverse using matrix multiplication
  m <- solve(data) %*% data
  
  # Set the inverse to the object
  x$setInverse(m)
  
  
  m
}
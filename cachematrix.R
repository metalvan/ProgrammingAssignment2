# These two functions work in conjunction to create a special matrix object that
# stores cached infromation. We then use cacheSolve which will solve the special
# matrix, or if it has already been solved, return the previous solution. Nifty.

# makeCacheMatrix takes a single inversable matrix as an argument, and outputs 
# a set of functions as a list available to that special matrix object.
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the inverted value of the matrix
# 4. get the inverted value of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL;
  
  set <- function(y) {
    x <<- y;
    m <<- NULL;
  };
  get <- function() x;
  setSolvedMatrix <- function(solved.matrix) m <<- solved.matrix;
  getSolvedMatrix <- function() m;
  
  output <- list(set=set, 
                 get=get, 
                 setSolvedMatrix=setSolvedMatrix, 
                 getSolvedMatrix=getSolvedMatrix);
  return(output)
}

# cacheSolve takes the "matrix" defined above as an argument and calls the solve 
# function on it and stores the result within the origina "matrix" you pass in. 
# If the "matrix" has already been solved, it does not call "solve" but instead
# returns the stored, previously solved matrix.

cacheSolve <- function(x, ...) {
  m <- x$getSolvedMatrix();
  if(!is.null(m)) {
    message("getting cached data");
    return(m);
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setSolvedMatrix(m)
  return(m)
}

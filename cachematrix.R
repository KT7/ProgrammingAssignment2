##These functions together store a matrix (assumed invertible) and its inverse, 
##computing the inverse only if it isn't previously cached, else it returns the cached
##inverse.

## makeCacheMatrix takes as input a square matrix (assumed nonsingular, i.e. invertible)
## and returns a list containing three functions

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL                              #Creates a 'space' to hold the matrix inverse and erases any cached inverses
  set <- function(y) {   
    x <<- y                              #Saves a matrix
    m <<- NULL                           #Erases any cached inverses
  }
  get <- function() x                    #returns stored matrix 
  setinv <- function(solve) m <<- solve  #Stores new inverse
  getinv <- function() m                 #Returns stored inverse
  list(set = set, get = get,             #output for use in cacheSolve
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve has as input the output of makeCacheMatrix, allowing it to return the matrix inverse by
## computation if not previously computed, otherwise it computes the inverse.

cacheSolve <- function(x, ...) {
  m <- x$getinv()                        #Obtains variable that could hold inverse from output of makeCachematrix
  if(!is.null(m)) {                      #If the inverse is in the output of makeCacheMatrix, this returns its 
                                         #value and prints a message indicating the inverse was previously cached
    message("getting cached data")
    return(m)
  }
  data <- x$get()                        #If inverse isn't cached, this obtains the matrix.
  m <- solve(data, ...)                  #This computes the matrix inverse.
  x$setinv(m)                            #This caches the inverse when not previously cached.
  m                                      #This outputs the inverse.
  
}

## This funciton creates an accessable way to solve for the inverse
## of a matrix, and cache it for later use to reduce the amount of
## work the computer must do when looking at the inverse of a large
## number of matrices.

## This function creates an object to cache the matrix, and it's
## inverse. It also creates functions that allow the user, or
## a program (in this case) to manipulate that list.

makeCacheMatrix <- function(x = matrix()) 
{
  ## The variable that will become the inverse
  in = NULL

  ## "set" will set the matrix to that of the
  ## new inputed matrix, and reset the inverse
  ## to null.
  set <- function(y) 
    {
      x <<- y
      in <<- NULL
    }

  ## This allows the user or other functions to
  ## get the original matrix.
  get <- function() { x }

  ## This allows the user or other functions to
  ## set the inverse to the current matrix.
  setin <- function(inverse) { in <<- inverse }

  ## This allows the user or other functions to 
  ## get the inverse of the current matrix.
  getin <- function() { in }

  ## This creates a list for the next function to
  ## use as a baseline to identify what it will do.
  list(set = set, get = get,
       setin = setin,
       getin = getin)
}


## This function takes the list of funtions and information
## of the specified matrix, checks to see if the inverse has
## been previously calculated, calculates the inverse if not,
## and returns the inverse.

cacheSolve <- function(x, ...) 
{
  
  ## This creates a local object containing the inverse
  ## of the current matrix.
  in <- x$getin()
  
  ## This check to see if the inverse is null (meaning it hasn't
  ## been calculated yet), and if it isn't, will return the cached
  ## inverse.
  if(!is.null(in)) {
      message("getting cached data")
      return(in)
  }
  
  ## This will create a local copy of the current matrix.
  data <- x$get()
  
  ## This will calculate the inverse of the current matrix,
  ## and reset the local copy of the inverse to the current
  ## version.
  in <- solve(data, ...)
  
  ## This sets the cached inverse of the current matrix equal
  ## to the newly calculated inverse.
  x$setin(in)
  
  ## Return the inverse.
  in
}
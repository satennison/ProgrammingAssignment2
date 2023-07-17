## appreciation to DanieleP[a3d81a0b7a4dfac963d508e11ea3d4520519a50c]
## for providing targeted guidance about interpreting this set-up.

## function locally stores 4 functions (similar to model function in HW2 assignment)
## function includes appropriate measures to set-up a square matrix
makeCacheMatrix <- function(x = matrix()){
      im <- NULL
      set <- function(y) {
            im <<- NULL
            sqcheck <- sqrt(length(y)) ##matrix must be square
            
            if (ceiling(sqcheck) == sqcheck) {
                  x <<- matrix(y,nrow=sqcheck) #if data is not as matrix, this reformats the data
            } else {
                  message("data reconfigured for square matrix; prepare to receive warning message")
                  square <- as.integer(sqcheck)+1
                  x <<- matrix(y,nrow=square)
                  ## this prepares a square matrix by adding additional spaces
                  ## and using the matrix function to recycle the numbers in the prompt
            }
      }
      get <- function() {
            if(any(is.na(x))) {
                  message("data missing; please input data matrix using '[FUN]$set'()")
                  ## message provides guidance about how to input data into matrix
            } else (x)
      }
      getim <- function() im
      setim <- function(solve) im <<- solve
      list(set = set, get = get,
           getim = getim, setim = setim)
}

## function evaluates inverse matrix used data from "makeCacheMatrix" function
cacheSolve <- function(x,...) {
      ## "x" must be an object containing "makeCacheMatrix" function
      im <- x$getim()
      if(!is.null(im)){
            message("retrieving cached data...")
            return(im)
      }
      message("calcuating...") #message indicates that inverted matrix is not cached
      data <- x$get()
      im <- solve(data,...)
      ## if matrix is not invertible, error message will appear
      ## "Lapack routine dgesv: system is exactly singular: U[#,#] = 0"
      x$setim(im)
      im
}
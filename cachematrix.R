## Put comments here that give an overall description of what your
## functions do
##  calculate the inverse of a matrix and cache it to be used again without having to recompute it.

## 

## Write a short comment describing this function

## MakeCacheMatrix is a function that creates a special "matrix" object that can cache its inverse. The function creates a list of 
## functions that do the following:
## to set the matrix
## to get the matrix
## to set the inverse matrix and, 
## to get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

        z <- NULL
        
        
  set <- function(y) {
       x <<- y
       z <<- NULL
       }
  
  get <- function() x
  setsolve <- function(solve) z <<- solve
  getsolve <- function() z
 
  
  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}

## Write a short comment describing this function
##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.  

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  z <- x$getsolve()
  if(!is.null(z)) {
   message("getting cached data")
  return(z)
  }
  data <- x$get()
  z <- solve(data)
  x$setsolve(z)
  z
   
}

##Tests: the function ensures that if the matrix changes the inverse changes as well

##Application to a matriz with dim 2x2:
a= matrix(c(1,1,-1,1), nrow=2,ncol=2)

mymatrix <- makeCacheMatrix(a)

cacheSolve(mymatrix)
cacheSolve(mymatrix)

###Application to a matrix with dim 3x3:
a= matrix(c(4,0,3,1,3,0,-1,2,7), nrow=3,ncol=3)

mymatrix <- makeCacheMatrix(a)

cacheSolve(mymatrix)
cacheSolve(mymatrix)

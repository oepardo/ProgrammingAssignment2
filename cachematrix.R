## This function first creates first creates a matrix object that caches its inverse through makeCacheMatrix 
## and then computes the inverse of that matrix returned by makeCacheMatrix

## This function sets the matrix, gets the matrix, sets the inverse of the matrix and gets it.

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
         }
  get<-function() x
  setinvmat<-function(solve) m<<- solve
  getinvmat<-function() m
  list(set=set, get=get,
       setinvmat=setinvmat,
       getinvmat=getinvmat)
}


## This function computes the matrix delivered by makeCacheMatrix, checks if teh inverse is already cached. 
## If not, calculates it, stores it in the cache and prints it

cacheSolve <- function(x, ...) {
  m<-x$getinvmat()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
        }
  mat <- x$get() 
  m<-solve(mat, ...)
  x$setinvmat(m)
  m
}

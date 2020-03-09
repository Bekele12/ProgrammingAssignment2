## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
   ## makeCacheMatrix takes matrix object that assume to be invertible i,e det(x) != 0
   ## and provide getter and setter method thru which it cache the computed invert matrix for subsqunet request, 
   ## but it set empty value during the first run of this function
makeCacheMatrix <- function(x = matrix()) {
 matInv <- NULL
      setMat <- function(mat){
          matInv <<- NULL
      }
      getMat <- function() x
      setInvMat <- function(solve) matInv <<- solve
      getInvMat <- function() matInv
      list(setMat = setMat,
           getMat = getMat,
           setInvMat = setInvMat,
           getInvMat = getInvMat)
}

xx <- makeCacheMatrix(A)
xx
yy <- cacheSolve(xx)
yy
debug(cacheSolve)


## Write a short comment describing this function
   ## Once makeCacheMatrix is executed, the element of the list(getter and setter) populated along with the formula 
   ## for computation of inverse mat. so the list element will be re-executed as per the underlying formula;
   ## and finally once the inverse computed, will cache it for next request
   

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    matInv <- x$getInvMat()
    print(matInv)
    t <- is.null(matInv)
    
    if(!is.null(matInv)){
     
        return(matInv)
    }
    mdata <- x$getMat()
    matInv <- solve(mdata,...)
    
    x$setInvMat(matInv)
    matInv
}


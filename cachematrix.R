## This set of functions will calculate the inverse of a square matrix. Since the inversion of a matrix
## is generally an expensive operation, the result of the inversion will be cached.
##
## USAGE NOTES:
## 
## 1. Create a square invertible matrix
##      testMatrix<-matrix(data=c(1,1,1,3,4,3,3,3,4),nrow=3,ncol=3)
## 2. Create the special matrix from the matrix created in 1 above.
##      splMatrix<-makeCacheMatrix(testMatrix)
## 3. Solve the matrix.
##      cacheSolve(splMatrix)
## 4. The solution happens because as yet nothing was stored in the cache. Solve the matrix again.
##      cacheSolve(splMatrix)
## 5. The matrix is solved to show the same results as in 4; however, a message is displayed to indicate
##    that the solution was taken from the cache.
##
##
## EXAMPLE
##
## > tm1<-matrix(data=c(1,1,1,3,4,3,3,3,4),nrow=3,ncol=3);sm1<-makeCacheMatrix(tm1)
## > cacheSolve(sm1)
##      [,1] [,2] [,3]
## [1,]    7   -3   -3
## [2,]   -1    1    0
## [3,]   -1    0    1
## > cacheSolve(sm1)
## [1] "Getting cached matrix"
##      [,1] [,2] [,3]
## [1,]    7   -3   -3
## [2,]   -1    1    0
## [3,]   -1    0    1

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 

## Take an input as a square invertible matrix and return a special matrix of functions
## to use the input matrix.
## 
##            CachedMatrix InvertedMatrix
## GetMethods getMatrix     getInvertedMatrix
## SetMethods setMatrix     setInvertedMatrix
## 
## http://r.789695.n4.nabble.com/Matrix-with-Functions-tp3258447p3258483.html

makeCacheMatrix <- function(inputMatrix = matrix()) {
  invertedMatrix <- NULL
  
  setMatrix <- function(newMatrix) {
    inputMatrix <<- newMatrix
    invertedMatrix <<- NULL
  }
  getMatrix <- function() inputMatrix 
  
  getInvertedMatrix<-function() invertedMatrix
  setInvertedMatrix<-function(invMatrix) {
    invertedMatrix <<- invMatrix
  }
  
  functionList<-list(getMatrix,setMatrix,getInvertedMatrix,setInvertedMatrix)
  specialMatrix<-matrix(functionList,nrow=2,ncol=2,
                        dimnames=list(c("GetMethods", "SetMethods"),c("CachedMatrix","InvertedMatrix")))
}

## Solve the input matrix to get the inverse ONLY if the calculation has not been done already.
## In which case, return from cache.

cacheSolve <- function(specialMatrix, ...) {
  cacheInvertedMatrix <- specialMatrix[["GetMethods","InvertedMatrix"]]()
  
  if (!is.null(cacheInvertedMatrix)) {
    print("Getting cached matrix")
    return(cacheInvertedMatrix)
  }
  
  cacheMatrix <- specialMatrix[["GetMethods","CachedMatrix"]]()
  cacheInvertedMatrix <- solve(cacheMatrix)
  specialMatrix[["SetMethods", "InvertedMatrix"]](cacheInvertedMatrix)
  
  cacheInvertedMatrix
}
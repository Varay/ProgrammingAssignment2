#Functions for calculating inversion of the matrix smart way (lazy evaluation + caching)

#'makeCacheMatrix' creates special matrix (list of functions) that is able to cache result of 'solve' function
#parameters:
#   currentMatrix - matrix to store
#result:
#   special matrix (to be used as a parameter for 'cacheSolve' function)
makeCacheMatrix <- function(currentMatrix = matrix()) {
    #cached value (inverted matrix)
    invertedMatrix <- NULL
    
    #matrix setter
    set <- function(newMatrix) {
        #store new matrix
        currentMatrix <<- newMatrix
        
        #invalidate old inversion of the matrix (because currentMatrix has changed)
        invertedMatrix <<- NULL
    }
    
    #matrix getter
    get <- function() {
        currentMatrix
    }
    
    #inverse matrix setter
    setinv <- function(inv) {
        invertedMatrix <<- inv
    }
    
    #inverse matrix getter
    getinv <- function() {
        invertedMatrix
    }
    
    #return list of functions
    list(set=set, get=get, setinv=setinv, getinv=getinv)
}

#'cacheSolve' calls solve on special matrix ('solve' function is called only once for given matrix)
#params:
#   cacheMatrix - special matrix created by 'makeCacheMatrix' function
#   ... - any other parameter will be passed to 'solve' function (it will only happen once - for given matrix)
#result:
#   inversion of matrix (result of 'solve' function)
cacheSolve <- function(cacheMatrix, ...) {
    
    #get a matrix that is the inverse of 'cacheMatrix'
    inv = cacheMatrix$getinv()
    
    if (is.null(inv)) {
        #inversion not yet computed
        
        #get original matrix
        mat = cacheMatrix$get()
        
        #calculate inversion
        invMat = solve(mat, ...)
        
        #cache result
        cacheMatrix$setinv(invMat)
        
        return(invMat)
    }
    else {
        #inversion already calculated, nothing to do
        return(inv)
    }
}

##John Earl P. Ebin
##R Programming Coursera Week 3 

MyMakeCacheMatrix <- function(m = matrix()) { ##input matrix function named matrix m
    inv <- NULL
    setmatrixm <- function(s){ ##setting function inverse 
      m <<- s
      inv <<- NULL
    }
    getfunctionm <- function() {m}
    setInversematrixm <- function(inverse) {inv <<- inverse} ##setting the inverse function matrix m
    getInversematrixm <- function() {inv} ##gettng the inverse matrix function m
    list(setmatrixm = setmatrixm, getfunctionm = getfunctionm, setInversematrixm = setInversematrixm, getInversematrixm = getInversematrixm)
}

MyCacheSolvefunction <- function(m, ...) {
    inv <- m$getInversematrixm()
    if(!is.null(inv)){
        message("Obtaining Information That Has Been Cached")
        return(inv)
    }
    matrixm <- m$getfunctionm()
    inv <- solve(matrixm, ...)  ##Solution for the inverse function m
    m$setInversematrixm(inv) ##Final output of matrix m = inverse matrix m 
    inv
}

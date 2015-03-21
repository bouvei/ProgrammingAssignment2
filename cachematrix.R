## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##example vectors from the assignment description
makeVector <- function(x = numeric()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmean <- function(mean) m <<- mean
    getmean <- function() m
    list(set = set, get = get,
         setmean = setmean,
         getmean = getmean)
}

cachemean <- function(x, ...) {
    m <- x$getmean()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- mean(data, ...)
    x$setmean(m)
    m
}
##End of exapmle functions from assignment description


makeCacheMatrix <- function(x = matrix()) {

    i <- null
    
    set <- function(matrix){
        m <<- matrix
        i <<- null
    }
    
    get <- function() x
    
    setInverse <- function(inverse) i <<- inverse
    
    getInverse <- function() i
    
    list(set=set, 
         get=get, 
         setInverse=setInverse, 
         getInverse=getInverse)
    
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ##get the cached inverted matrix
    m <- x$getInverse()
    
    ## if the inverse matrix is already cached, retrieve and return it
    if(!is.null(m)){
        message("already set-cached, getting cached inverse matrix")
        return(m)
    }
    
    else{
        ##allocate the matrix to data
        data <- x$get()
    
        ##inverted matrix calculation with solve function
        m <- solve(data) %*% data
        
        ##set the x with calculated inverted matrix
        x$setInverse(m)
        
        ##return the inverted matrix if it's calculated from scratch
        return(m)
    }
}

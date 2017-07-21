## This function compute the inverse of the matrix and stores
## the result in the cache and so does not recalculate the inverse
##but retrieve it from the cache

## The makeCacheMatrix function store a vector and its inverse

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set<- function(y){
                x<<-y
                m<<-NULL
        }
        get<-function() x
        setsolve<- function(solve) m<<- solve
        getsolve<-function() m
        list(set = set, get=get,setsolve=setsolve,getsolve=getsolve)
        
}


## The cachesolve function retrieves the inverse of the matrix in cache

cacheSolve <- function(x, ...) {
        m<-x$getsolve()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data<-x$get()
        m<- solve(data,...)
        x$setsolve(m)
        ## Return a matrix that is the inverse of 'x'
        m
        
}


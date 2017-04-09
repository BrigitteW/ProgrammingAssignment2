##These functions will create a matrix object that can cache its inverse and then solve for the inverse.
##If the inverse has already been calculated then the second function, cacheSolve, will retrieve it from the cache.


##The following function will create a matrix that can cache its inverse

makeCacheMatrix<-function(x=matrix()){
        i<-NULL
        set<-function(y){
                x<<-y
                i<<-NULL
        }
        get<-function()x
        setsolve<-function(solve) i<<-solve
        getsolve<-function() i
        list(set=set,get=get,
             setsolve = setsolve,
             getsolve = getsolve)
}

##The following function will determine if the inverse of the matrix has already been calculated to retrieve from cache

cacheSolve<-function(x,...){
        ##Return a matrix that is the inverse of 'x'
        
        i<-x$getsolve()
        if(!is.null(i)){
                message("getting cached data")
                return(i)
        }
        data<-x$get()
        i<-solve(data,...)
        x$setsolve(i)
        i
}

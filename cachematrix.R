## makeCacheMatrix creates a special vector which is a list containing a function to 
## set the value of a vector
## get the value of a vector
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL 
        set <- function(y) { 
                x <<- y 
                m <<- NULL 
        } 
        get <- function() x 
        setinverse <- function(solve) m <<- solve 
        getinverse <- function() m 
        list(set = set, get = get, 
        setinverse = setinverse, 
        getinverse = getinverse) 
  
}


## the cacheSolve function calculates the inverse of the special vector create by the above function
## It first checks to see if it's been calculated.  If so it gets the inverse from
## the cche and skips the computation.  Otherwise it calculates the inverse and sets the value
## of the inverse in the cache via the set inverse function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
          message("getting cached data")
        return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}


## use example on 2 by 2:   a <- matrix(c(r1c1,r2c1,r1c2,r2,c2),nrow=2,ncol=2)
## b <- makeCacheMatrix(a)
## c <- cacheSolve(b)


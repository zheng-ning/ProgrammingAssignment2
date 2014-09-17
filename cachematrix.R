## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## acheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.
makeCacheMatrix <- function(x = matrix()) { 
        m <- NULL # declare a cariable named m and assign it to NULL
        set <- function(y) { # create a function named set takes argument of y
                x <<- y # assign y to x
                m <<- NULL # what is m and where is m come from?
        }
        get <- function() x # x is the return value
        setsolve <- function(solve) m <<- mean # takes mean as arg return m
        getsolve <- function() m # return m 
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

cacheSolve <- function(x, ...) { 
# In this specific case it gives us ability to pass additional arguments to mean function
        m <- x$getsolve() # getmean x
        if(!is.null(m)) { # check if it is empty
                message("getting cached data")
                return(m)
        }
        data <- x$get() # get data 
        m <- solve(data, ...) # calculate mean
        x$setsolve(m) # set mean
        m
}
## R Programming  (Johns Hopkins University / Coursera)
## July 6, 2015 - August 1, 2015
## taught by Roger D. Peng, PhD, Jeff Leek, PhD, Brian Caffo, PhD

## Programming Assignment 2: Lexical Scoping

## The functions below implement a structure to cache the potentially
## compute-intensive matrix inversion. A base matrix is stored and
## functions are implemented to set/get the matrix and set/get its
## matrix inverse. The "solved" matrix (inversion) is cached in the
## parent environment for use without recalculation as needed until
## the matrix changes.

makeCacheMatrix <- function(x = matrix()) {
## This creates a special "matrix" object that can cache its inverse.
## The function accepts a matrix x as its argument, initializes
## a location m where the inverse will be cached, and implements the following
## functions:
##         set - stores the base matrix in the parent environment and clears
##               the cache;
##         get - recalls the base matrix;
##         setinverse - stores the inverse of the base matrix in cache;
##         getinverse - returns the cached matrix inverse or NULL if the
##               current base matrix has not yet been solved.
#################################################
        
        # Reserve a location m to cache the inverted matrix calculated later.
        # Note: Our empty value is NULL and is.matrix() will check whether
        #       a value has been calculated and set or not for current matrix.
        m <- NULL
        
        #FUNCTIONS:
        # SET accepts a matrix argument
        #  1) Store the input matrix in the parent environment.
        #  2) Clear the cache variable (matrix inverse not yet calculated).
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        # GET
        #  1) Return the matrix variable (pulls x from parent environment)
        get <- function() x
        
        # SETINVERSE accepts a matrix argument
        #  1) Store the passed matrix inverse in the parent environment.
        setinverse <- function(matinv) m <<- matinv
        
        # GETINVERSE
        #  1) Return the matrix inverse cache from parent environment. Will 
        #     return NULL if inverse has not yet been calculated.
        getinverse <- function() m

                
        # Initialize and return the CacheMatrix list structure
        list( set = set, get = get, setinverse = setinverse, 
              getinverse = getinverse
        )
}


cacheSolve <- function(x, ...) {
## This computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then we retrieve the inverse from the cache.
## The function returns either a cached or newly calculated matrix
## inverse value for our "special" CacheMatrix variables. It first checks
## whether a value has been cached and if so returns that value. Otherwise it
## calculates the matrix inverse (solve()) and returns this inverse.
## Passed argument x created by above function makeCacheMatrix.
#################################################

        # load the inverse value
        m <- x$getinverse()
        if (is.matrix(m)) {
                # Since NULL wasn't found we have valid cache.
                message("getting cached data")
                return(m)
                # Done!
        }
        
        # Can only arrive here when NULL is returned from x$getinverse
        # indicating matrix inverse was not yet calculated.
        mymat <- x$get()  
        m <- solve(mymat)
        x$setinverse(m)   # Cache the matrix inverse
        m
}
## The function here is used to cache the matrix in cache or memory
## And retrieve it back from memory to benefit the performance 
## and calculations are boundary driven within the scope of memory

## makeCachematrix is used for initializing the matrix in memory

makeCacheMatrix <- function(x = matrix()) {
    xInverse <- NULL # Inversion Initialize
    set <- function(y) {
      x<<-y # Assign Values
      xInverse <- NULL # Inversion Initialize
    }
    get <- function() x # Get Matrix
    setInverse <- function(Inverse) xInverse <<- Inverse # Set Inverse Matrix
    getInverse <- function() xInverse # Output Inverse Matrix
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## CacheSolve Function is used for derive Inverse using Solve and produce results via memory if results exists in cache.

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
    mtrx <- x$getInverse() # Get Matrix
## First time this would be NULL due to xInverse <- NULL setting
    if(!is.null(mtrx)) {
      message("Getting results from cache ....")
      return(mtrx)
    }
    cacheLoad<-x$get() # Get and Store in cacheLoad Variable
    mtrx <- solve(cacheLoad) # solve equation inverse
    x$setInverse(mtrx)
    mtrx
}

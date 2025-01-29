# Function to create a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL  # Initialize the inverse as NULL
    set <- function(y) {
        x <<- y  # Set the value of the matrix
        inv <<- NULL  # Reset the inverse cache
    }
    get <- function() x  # Get the value of the matrix
    setInverse <- function(inverse) inv <<- inverse  # Set the value of the inverse
    getInverse <- function() inv  # Get the value of the inverse
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)  # Return a list of the functions
}

# Function to compute the inverse of the special "matrix" returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
    inv <- x$getInverse()  # Get the cached inverse
    if(!is.null(inv)) {
        message("getting cached data")  # If cached, return the inverse
        return(inv)
    }
    data <- x$get()  # Get the matrix data
    inv <- solve(data, ...)  # Compute the inverse
    x$setInverse(inv)  # Cache the inverse
    inv  # Return the inverse
}

# Function to create a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(mat = matrix()) {
  inverse <- NULL
  set <- function(new_matrix) {
    mat <<- new_matrix
    inverse <<- NULL  # Reset the cached inverse when the matrix changes
  }
  get <- function() mat
  set_inverse <- function(new_inverse) inverse <<- new_inverse
  get_inverse <- function() inverse
  list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)
}

# Function to compute the inverse of the special "matrix" and cache the result
cacheSolve <- function(cacheMatrix) {
  # Check if the cached inverse exists and is up-to-date
  inverse <- cacheMatrix$get_inverse()
  if (!is.null(inverse)) {
    message("Getting cached data")
    return(inverse)
  }
  
  # If not, compute the inverse using solve and cache the result
  mat <- cacheMatrix$get()
  if (!is.null(mat) && all(dim(mat)[1] == dim(mat)[2])) {
    inverse <- solve(mat)
    cacheMatrix$set_inverse(inverse)
    return(inverse)
  } else {
    stop("Input is not a square invertible matrix.")
  }
}

# Example usage:
# Create a special "matrix" object
myMatrix <- makeCacheMatrix(matrix(c(4, 2, 3, 1), nrow = 2))

# Compute the inverse and cache the result
inverse_result <- cacheSolve(myMatrix)
print("Inverse:")
print(inverse_result)

# Retrieve the cached inverse without recomputing
cached_inverse <- cacheSolve(myMatrix)
print("Cached Inverse:")
print(cached_inverse)

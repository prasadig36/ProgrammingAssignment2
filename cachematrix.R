##  Function to find out the inverse of a matrix. Inverse is cached once calculated.



## Create a list which can set / retrieve the matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
Inverse_matrix <- NULL
    matrix_inverse <- NULL
    set_mat <- function(y) {
      x <<- y
      matrix_inverse <<- NULL
    }
    get_mat <- function() x

    set_inverse <- function(matrix_inv) matrix_inverse <<- matrix_inv

    get_inverse <- function() matrix_inverse
    
    list(set = set_mat, get = get_mat,
         set_inv = set_inverse,
         get_inv = get_inverse)
}


## Function to get the inverse of a matrix from the cached value if present else save it to cache.

cacheSolve <- function(x, ...) {
        inv_mat = x$get_inv()
        if(!is.null(inv_mat))
        {
          print("Getting cached inverse data")
          return(inv_mat)
        }
        else
        {
          m <- x$get()
          inv_mat = solve(m)
          x$set_inv(inv_mat)
          inv_mat
        }
}

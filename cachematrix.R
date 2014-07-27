## Estas dos funciones calcularan la inversa 
## de una matriz tomando en cuenta en primer
## lugar almacenamiento de la matriz en el entorno
## es decir, la obtencin de la matriz como insumo
## para la inversa.


# Esta funcion almacena la matriz
makeCacheMatrix<-function(x=matrix()){
    m <-NULL
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inversa) m <<- inversa
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

#Esta funcion calcula la inversa de la matriz

cacheSolve <- function(x, ...) {
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

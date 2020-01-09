## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function()x
        setinverse <- function(inverse) m<<- inverse
        getinverse <- function()m
        list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        data <- x$get() #bring matrix from Global Environment
        m_ret <- x$getinverse() #bring cache from 'm' and store it in m_ret
        if(!is.null(m_ret)==T){ #if there is cache in m_ret
                message('getting cached data') #send message
                return(m_ret)
        }else{ #or
                m_inversed <- solve(data,...) #calculate inverse matrix
                x$setinverse(m_inversed) #store calculated inverse matrix in 'm'
        }
        m_inversed
}

#Now test fucntion!
m1 <- matrix(c(1/8,1/2,1/4,1/5),2,2)
n1 <- solve(m1)
makeCacheMatrix.obj <- makeCacheMatrix(m1)
solved1 <- cacheSolve(makeCacheMatrix.obj)
solved1
identical(n1,solved1) #TRUE

m2 <- matrix(c(2,3,4,5),2,2)
n2 <- solve(m2)
makeCacheMatrix.obj$set(m2) #setting new matrix
solved2 <- cacheSolve(makeCacheMatrix.obj)
solved2
identical(n2,solved2) #TRUE

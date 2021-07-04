##Below are two functions that are used to create a special object 
##that stores a matrix and cache its inverse matrix

#The first function, makeCacheMatrix creates a special "vector", 
#which is really a list containing a function to
#       1. set the value of the matrix
#       2. get the value of the matrix
#       3. set the value of the inverse of matrix
#       4. get the value of the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
        
        inv_x<-NULL
        
        set<-function(y){    #sets the value of x
                x<<-y
                inv_x <<-NULL
        }
        
        get<-function() x    #gets the value of x
        
        setinverse<- function(inverse) inv_x<<-inverse #sets the value of inverse matrix of x
        
        getinverse<- function ()inv_x #gets the value of inverse matrix of x
 
        #Return the List       
        list(set=set, 
             get=get,
             setinverse=setinverse,
             getinverse=getinverse)
}


## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
        
        inv_x<- x$getinverse() #check for inverse matrix in cache
        
        #if inv_x is cached, return cached matrix
        if(!is.null(inv_x)) {
                message("getting cached data")
                print(inv_x)  #print the inverse
                return(inv_x) #return the inverse
        }
        
        #if inv_x is not cached, set inverse matrix in cache and return
        mdata<-x$get()
        inv_x  <-solve(mdata) #calculating the inverse
        x$setinverse(inv_x)  #setting the inverse
        print(inv_x)            #printing the inverse
        return(inv_x)           #return the inverse
}

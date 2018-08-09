# this should return a list of 4 functions,
# set, get, setInverse, getInverse
# it will take a matrix as a parameter

makeCacheMatrix <- function(aMatrix){
  # original matrix will be passed on to normalMatrix
  normalMatrix <- NULL
  
  # inversed matrix will be passed on to inversedmatrix
  inversedMatrix <- NULL
  
  # a boolen to check if handle
  handled <- FALSE
  
  # functions for the original matrix
  set <- function(aMatrix){
    # just a check if a matrix has been supplied
    if(class(aMatrix)=='matrix'){
      normalMatrix <<- aMatrix
    }else{
      message('please provide a valid matrix')
    }
  }
  set(aMatrix) #calling it automatically
  get <- function() normalMatrix
  
  # functions for the inversed matrix
  setInverse <- function(iMatrix){
    inversedMatrix <<- iMatrix
  }

  getInverse <- function() inversedMatrix
  
  # return this list with all these functions inside it
  list(s=set,g=get,si=setInverse,gi=getInverse)
}

# this would take the list containing all the four functions we have created
# we will then have access to the setInverse and getInverse
cacheSolve <- function(aMakeCacheMatrix){
  # first check if inverseMatrix is still null
  # this means the inverse of the normal matrix has not been solved yet
  print(is.null(aMakeCacheMatrix$gi()))
  
  if(is.null(aMakeCacheMatrix$gi())){
    print('solved for the first time')
    # retrieves the uninversed matrix, places it in solve func then assign it to 
    # solvedMatrix
    solveMatrix <- solve(aMakeCacheMatrix$g())
    
    # sets the inversedMatrix to the solvedMatrix for later extraction
    aMakeCacheMatrix$si(solveMatrix)
    
    # just a call to see what the inverse matrix looks like
    aMakeCacheMatrix$gi()
  }else{
    # a test if caches has indeed been completed
    print('inverse has previously been solved')
    
    # "solve" is no longer invoked here, we only asked for the existing 
    # inversedMatrix content
    aMakeCacheMatrix$gi()
  }
}

# Let's try it out

# we'll create a sample matrix
mt1 <- matrix(c(19,21,3,14,5,6,47,8,9,10,32,51,3,14,15,26),4,4)

# let's set MCM as our makeCacheMatrix with mt1
MCM <- makeCacheMatrix(mt1)

# first run on cacheSolve should print solved for the first time
CM <- cacheSolve(MCM)
CM

# second run should now tell us that it was previously solved and just
# gives us the content of what was previously set as inversedMatrix
CM <- cacheSolve(MCM)
CM

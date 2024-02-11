#Load libraries
library(tidyverse)
library(abind)


#----- Create array of matrices from dataframe -----#
makeArray = function(df){
  matArray = array(dim = c(3, 3, 0))
  
  for(i in 1:nrow(df)){
    temp = matrix(testData[i,2:10], ncol = 3, byrow=F) #Will need to adjust for actual data.
    matArray = abind(matArray, temp, along = 3)
  }
  
  return(matArray)
}

# Usage
# testDataArray = makeArray(testData)
# testDataArray[,,5]


#----- Mean matrix from matrix array -----#
meanMat = function(array){
  length = dim(array)[3]
  matSum = apply(array, c(1, 2), sum)
  oBar = matSum / length
  
  V = svd(oBar)$u
  W = svd(oBar)$v
  M = V %*% t(W)
  
  return(M)
}

#Usage
#meanMat(testDataArray)


#----- Average Misorientation Angle -----#
misorientAngle = function(array, obs=NA){
  
  if(is.na(obs) == T){
    
    arrayLength = dim(array)[3]
    sumAngle = 0
    M = meanMat(array)
    
    for(i in 1:arrayLength){
      oP = t(array[,,i])
      traceOPM = sum(diag(oP %*% M)) 
      sumAngle = sumAngle + acos((traceOPM-1)/2)
    }
    
    avgAngle = sumAngle/arrayLength
    cat("Average misorientation angle:\n")
    return(avgAngle)
    
  }else{
    
    oP = t(array[,,obs])
    M = meanMat(array)
    traceOPM = sum(diag(oP %*% M)) 
    angle = acos((traceOPM-1)/2)
    
    cat(paste0("Misorientaton angle of observation ", as.character(obs), ":\n"))
    return(angle)
    
  }

}

#Usage
#misorientAngle(testDataArray) #Average angle
#misorientAngle(testDataArray, obs=10) #Angle of individual observation







#Load libraries/optional testing data
#testData = read.csv(paste0(getwd(),"/files/Data.csv"))
library(tidyverse)
library(abind)


#----- Create array of matrices from dataframe -----#
makeArray = function(df){
  matArray = array(dim = c(3, 3, 0))
  
  for(i in 1:nrow(df)){
    temp = matrix(testData[i,4:12], ncol = 3, byrow=F)
    matArray = abind(matArray, temp, along = 3)
  }
  
  colnames(matArray) = c("[X]", "[Y]", "[Z]")
  rownames(matArray) = c("[1]", "[2]", "[3]")
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
misorientAngle = function(array, verbose = T){

  arrayLength = dim(array)[3]
  sumAngle = 0
  M = meanMat(array)
  
  for(i in 1:arrayLength){
    oP = t(array[,,i])
    traceOPM = sum(diag(oP %*% M)) 
    sumAngle = sumAngle + acos((traceOPM-1)/2)
  }
  
  avgAngle = sumAngle/arrayLength
  if(verbose == T){
    cat("Average misorientation angle:\n")
  }
  return(avgAngle)
}

#Usage
#misorientAngle(testDataArray) #Average angle
#misorientAngle(testDataArray, obs=10) #Angle of individual observation


#----- Permutation Test -----#
arrayPerm = function(array1, array2, nperms = 1000, alt = "two.sided", silent = F){
  
  #combine matrices
  combinedMats = abind(array1,array2)
  
  #observed difference
  meanmats = array(dim = c(3, 3, 2))
  meanmats[,,1] = meanMat(array1)
  meanmats[,,2] = meanMat(array2)
  diffObs = misorientAngle(meanmats, verbose = F)

  #permutation test
  perms = c()
  for(i in 1:nperms){
    index=sample(1:dim(combinedMats)[3],dim(array1)[3]) 
    permA=combinedMats[,,index]
    permB=combinedMats[,,-index]
    
    tempArray = array(dim = c(3, 3, 2))
    tempArray[,,1] = meanMat(permA)
    tempArray[,,2] = meanMat(permB)
    
    perms[i] = misorientAngle(tempArray, verbose = F)
  }
  
  
  #Conclusion
  pval = NA
  
  if(alt == "two.sided"){
    #Two-tailed p-value: proportion of abs(permutations) at or more extreme than abs(observed)
    pval = length(which(abs(perms)>=abs(diffObs)))/length(perms)
  }
  
  if(alt == "greater"){
    #right-tailed p-value: proportion of permutations at or more extreme than observed
    pval = length(which(perms>=diffObs))/length(perms)
  }
  
  if(alt == "less"){
    #left-tailed p-value: proportion of permutations at or less extreme than observed
    pval = length(which(perms>=diffObs))/length(perms)
  }
  
  if(silent == F){
    cat("Observed misorientation angle =",diffObs,
    "\nP-value =",pval)
    invisible(list(diffObs,pval))

  }else{
    return(list(diffObs,pval))
  }
}




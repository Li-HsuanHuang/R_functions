genTransMatDis = function(stateList){
  transMat = c()
  
  for(i in 1:length(stateList)){
    
    currentTimeSeries = stateList[[i]]
    seriesStateUnique = unique(currentTimeSeries)
    sortUnique = sort(seriesStateUnique)
    transStates = colnames(transMat)
    
    
    if(!all(seriesStateUnique %in% transStates))
    {
      newState = sort(as.numeric(union(seriesStateUnique,transStates)))
      newStateLength = length(newState)
      
      tempTrans = transMat
      
      transMat = matrix(0,nrow=newStateLength,ncol=newStateLength)
      sortRowNames = rownames(transMat) = newState
      sortColNames = colnames(transMat) = newState
      
      #print("temptrans:")
      #print(tempTrans)
      #print(newState)
      #print(transMat)
      if(!is.null(tempTrans)){
        #print(i)
        transMat = matSubMatch(tempTrans, transMat)  
      }
      
    }
    #print(transMat)
    
    for (j in 1:length(currentTimeSeries)-1){
      obsLocation = c( match(currentTimeSeries[j],sortRowNames), match(currentTimeSeries[j+1],sortColNames) )
      #print(obsLocation)
      transMat[obsLocation[1],obsLocation[2]] = transMat[obsLocation[1],obsLocation[2]] + 1
    }
    #print(transMat)
  }
  return(transMat)
}
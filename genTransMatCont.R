genTransMatCont = function(stateList,timeList){
  if (length(timeList) != length(stateList) ){
    sprintf("uneven number of time series observations in time and state matricies")
    return(NULL)
    
  }
  
  
  for (i in 1:length(testStateList)){
    
    if (  length(timeList[[i]]) != length(stateList[[i]]) ){
      sprintf("uneven number of time and state observations in row: %i",i)
      return(NULL)
      
    }
    
    if (time=="continuous"){
      ind = which(diff(timeSeries)!=0)
      if (length(ind)!=0){
        timeSeries = c(timeSeries[ind],tail(timeSeries,1))
      }
    }
    
    m = length(unique(timeSeries))
    transmat = matrix(0,nrow=m,ncol=m)
    
    sortRowNames = rownames(transmat) = sort(unique(timeSeries))
    sortColNames = colnames(transmat) = sort(unique(timeSeries))
    timeSeries = as.character(timeSeries)
    
    for (j in 1:(length(timeSeries)-1)){
      obsLocation = c( match(timeSeries[j],sortRowNames), match(timeSeries[j+1],sortColNames) )
      transmat[obsLocation[1],obsLocation[2]] = transmat[obsLocation[1],obsLocation[2]] + 1
    }  
  }
  
  return(transmat)
}
